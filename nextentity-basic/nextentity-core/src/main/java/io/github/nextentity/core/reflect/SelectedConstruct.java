package io.github.nextentity.core.reflect;

import io.github.nextentity.core.ExpressionTypeResolver;
import io.github.nextentity.core.api.expression.BaseExpression;
import io.github.nextentity.core.api.expression.EntityPath;
import io.github.nextentity.core.api.expression.QueryStructure.Selected;
import io.github.nextentity.core.meta.AssociationAttribute;
import io.github.nextentity.core.meta.BasicAttribute;
import io.github.nextentity.core.meta.EntitySchema;
import io.github.nextentity.core.meta.EntityType;
import io.github.nextentity.core.meta.ProjectionAssociationAttribute;
import io.github.nextentity.core.meta.ProjectionBasicAttribute;
import io.github.nextentity.core.meta.ProjectionType;
import io.github.nextentity.core.reflect.schema.Schema;
import lombok.Getter;
import lombok.experimental.Accessors;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static io.github.nextentity.core.api.expression.QueryStructure.Selected.SelectArray;
import static io.github.nextentity.core.api.expression.QueryStructure.Selected.SelectEntity;
import static io.github.nextentity.core.api.expression.QueryStructure.Selected.SelectPrimitive;
import static io.github.nextentity.core.api.expression.QueryStructure.Selected.SelectProjection;

@Accessors(fluent = true)
public class SelectedConstruct implements ObjectFactory {

    @Getter
    private final List<BaseExpression> selects;
    private final ObjectFactory constructor;

    public SelectedConstruct(List<BaseExpression> selects, ObjectFactory constructor) {
        this.selects = selects;
        this.constructor = constructor;
    }

    public Object get(Arguments arguments) {
        return constructor.get(arguments);
    }

    public static SelectedConstruct of(EntityType entityType, Selected selected, boolean expandObjectAttribute) {
        return new Builder(entityType, expandObjectAttribute).build(selected);
    }

    @Override
    public Class<?> type() {
        return constructor.type();
    }

    private static final class Builder {
        EntityType entityType;
        boolean expandPath;
        List<BaseExpression> selects = new ArrayList<>();

        public Builder(EntityType entityType, boolean expandAssociationAttribute) {
            this.entityType = entityType;
            this.expandPath = expandAssociationAttribute;
        }

        SelectedConstruct build(Selected selected) {
            ObjectFactory schemaConstructor = createObjectConstructor(selected);
            return new SelectedConstruct(Collections.unmodifiableList(selects), schemaConstructor);
        }

        private ObjectFactory createObjectConstructor(Selected selected) {
            if (selected instanceof SelectEntity) {
                return select(((SelectEntity) selected));
            } else if (selected instanceof SelectPrimitive) {
                return select((SelectPrimitive) selected);
            } else if (selected instanceof SelectArray) {
                return select((SelectArray) selected);
            } else if (selected instanceof SelectProjection) {
                return select((SelectProjection) selected);
            } else {
                throw new UnsupportedOperationException("Unsupported selected type: " + selected.getClass().getName());
            }
        }

        private ObjectResult select(SelectEntity entity) {
            return selectEntity(new ObjectResult(), entityType, entity.fetch());
        }

        private <T extends ObjectResult> T selectEntity(T objectResult, EntitySchema schema) {
            return selectEntity(objectResult, schema, Collections.emptyList());
        }

        private <T extends ObjectResult> T selectEntity(T result, EntitySchema schema, Collection<? extends EntityPath> fetch) {
            Collection<? extends BasicAttribute> attributes = schema.attributes();
            List<ObjectAttributeFaced> items = new ArrayList<>();
            for (BasicAttribute attr : attributes) {
                if (attr.isPrimitive()) {
                    IndexableBasicAttribute attribute = getIndexableBasicAttribute(attr);
                    items.add(attribute);
                }
            }
            result.attributes(items);
            if (fetch != null && !fetch.isEmpty()) {
                Map<EntitySchema, ObjectResult> map = new HashMap<>();
                map.put(schema, result);
                for (EntityPath path : fetch) {
                    Schema cur = schema;
                    for (String s : path) {
                        BasicAttribute attribute = ((EntitySchema) cur).getAttribute(s);
                        cur = attribute;
                        if (attribute instanceof AssociationAttribute) {
                            map.computeIfAbsent((AssociationAttribute) attribute, k -> {
                                ObjectAttribute v = selectEntity(new ObjectAttribute(), k).attribute(attribute);
                                ObjectResult declaring = map.get(attribute.declareBy());
                                declaring.addAttribute(v);
                                return v;
                            });
                        } else {
                            IndexableBasicAttribute item = getIndexableBasicAttribute(attribute);
                            map.get(attribute.declareBy()).addAttribute(item);
                        }
                    }
                }
            }
            result.type(schema.type());
            return result;
        }

        private ObjectResult select(SelectProjection selected) {
            Class<?> type = selected.type();
            ProjectionType projection = entityType.getProjection(type);
            return selectProjection(projection);
        }

        private ObjectResult selectProjection(ProjectionType projection) {
            return selectProjection(new ObjectResult(), projection, true);
        }

        private <T extends ObjectResult> T selectProjection(T result, ProjectionType projection, boolean includeObject) {
            Collection<? extends ProjectionBasicAttribute> properties = projection.attributes();
            ArrayList<ObjectAttributeFaced> items = new ArrayList<>(properties.size());
            for (ProjectionBasicAttribute attribute : properties) {
                if (attribute.isPrimitive()) {
                    IndexableProjectionAttribute item = getIndexableProjectionAttribute(attribute);
                    items.add(item);
                } else if (attribute.isObject() && includeObject) {
                    ProjectionAssociationAttribute schema = (ProjectionAssociationAttribute) attribute;
                    if (attribute.deep() <= 8) {
                        ObjectAttribute item = selectProjection(
                                new ObjectAttribute(), schema, !schema.circularReferenced())
                                .attribute(attribute);
                        items.add(item);
                    }
                }
            }
            result.type(projection.type()).attributes(items);
            return result;
        }


        private ArrayResult select(SelectArray selected) {
            Collection<? extends Selected> items = selected.items();
            List<ObjectFactory> list = new ArrayList<>(items.size());
            for (Selected item : items) {
                ObjectFactory objectConstructor = createObjectConstructor(item);
                list.add(objectConstructor);
            }
            return new ArrayResult().items(list);
        }

        private ObjectFactory select(SelectPrimitive selected) {
            BaseExpression expression = selected.expression();
            if (expandPath && expression instanceof EntityPath) {
                return selectEntityPath(entityType, (EntityPath) expression);
            } else {
                return getIndexable(expression);
            }
        }

        private ObjectAttributeFaced selectEntityPath(EntitySchema entityType, EntityPath path) {
            BasicAttribute attribute = entityType.getAttribute(path);
            if (attribute.isPrimitive()) {
                return getIndexableBasicAttribute(attribute);
            } else if (attribute.isObject()) {
                ObjectAttribute result = new ObjectAttribute().attribute(attribute);
                return selectEntity(result, (EntitySchema) attribute);
            } else {
                throw new UnsupportedOperationException();
            }
        }

        private IndexableProjectionAttribute getIndexableProjectionAttribute(ProjectionBasicAttribute attribute) {
            IndexableProjectionAttribute item = new IndexableProjectionAttribute()
                    .index(selects.size())
                    .attribute(attribute);
            selects.add(attribute.entityAttribute().path());
            return item;
        }

        private Indexable getIndexable(BaseExpression expression) {
            Indexable index = new Indexable()
                    .index(selects.size())
                    .type(ExpressionTypeResolver.getExpressionType(expression, entityType));
            selects.add(expression);
            return index;
        }

        private IndexableBasicAttribute getIndexableBasicAttribute(BasicAttribute attribute) {
            IndexableBasicAttribute result = new IndexableBasicAttribute()
                    .index(selects.size())
                    .attribute(attribute);
            selects.add(attribute.path());
            return result;
        }
    }


}
