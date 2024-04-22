package io.github.nextentity.core.reflect;

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
import io.github.nextentity.core.reflect.schema.Attribute;
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

    private static final class Builder {
        EntityType entityType;
        boolean expandReferencePath;
        List<BaseExpression> selects = new ArrayList<>();

        public Builder(EntityType entityType, boolean expandReferencePath) {
            this.entityType = entityType;
            this.expandReferencePath = expandReferencePath;
        }

        SelectedConstruct build(Selected selected) {
            ObjectFactory schemaConstructor = createObjectConstructor(selected);
            return new SelectedConstruct(Collections.unmodifiableList(selects), schemaConstructor);
        }

        private AttributeConstructor createObjectConstructor(Selected selected) {
            if (selected instanceof SelectEntity) {
                return selectEntity(((SelectEntity) selected));
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

        private AttributeConstructor selectEntity(SelectEntity entity) {
            return selectEntity(entityType, entity.fetch());
        }

        private ObjectResult selectEntity(EntitySchema schema) {
            return selectEntity(schema, Collections.emptyList());
        }

        private ObjectResult selectEntity(EntitySchema schema, Collection<? extends EntityPath> fetch) {
            Collection<? extends BasicAttribute> properties = schema.attributes();
            List<AttributeConstructor> items = new ArrayList<>();
            for (BasicAttribute property : properties) {
                if (property.isPrimitive()) {
                    items.add(indexable(property));
                }
            }
            ObjectResult result = new ObjectResult().attributes(items);
            if (fetch != null && !fetch.isEmpty()) {
                Map<EntitySchema, ObjectResult> map = new HashMap<>();
                map.put(schema, result);
                for (EntityPath path : fetch) {
                    Schema cur = schema;
                    for (String s : path) {
                        BasicAttribute property = ((EntitySchema) cur).getAttribute(s);
                        cur = property;
                        if (property instanceof AssociationAttribute) {
                            map.computeIfAbsent((AssociationAttribute) property, k -> {
                                ObjectResult v = selectEntity(k).attribute(property);
                                ObjectResult declaring = map.get(property.declareBy());
                                declaring.addProperty(v);
                                return v;
                            });
                        } else {
                            IndexableProperty item = indexable(property);
                            map.get(property.declareBy()).addProperty(item);
                        }
                    }
                }
            }
            return result.type(schema.type());
        }

        private IndexableProperty indexable(BasicAttribute property) {
            return mewIndexableProperty(property.path()).attribute(property);
        }

        private IndexableProperty mewIndexableProperty(BaseExpression expression) {
            IndexableProperty property = new IndexableProperty().index(selects.size());
            selects.add(expression);
            return property;
        }

        private AttributeConstructor select(SelectProjection selected) {
            Class<?> type = selected.type();
            ProjectionType projection = entityType.getProjection(type);
            return selectProjection(projection);
        }

        private ObjectResult selectProjection(ProjectionType projection) {
            return selectProjection(projection, true);
        }

        private ObjectResult selectProjection(ProjectionType projection, boolean includeObject) {
            Collection<? extends ProjectionBasicAttribute> properties = projection.attributes();
            ArrayList<AttributeConstructor> items = new ArrayList<>(properties.size());
            for (ProjectionBasicAttribute property : properties) {
                if (property.isPrimitive()) {
                    items.add(mewIndexableProperty(property.entityAttribute().path()).attribute(property));
                } else if (property.isObject() && includeObject) {
                    ProjectionAssociationAttribute schema = (ProjectionAssociationAttribute) property;
                    if (property.deep() <= 8) {
                        items.add(selectProjection(schema, !schema.circularReferenced()));
                    }
                }
            }
            return new ObjectResult()
                    .attribute(projection instanceof Attribute ? (Attribute) projection : null)
                    .type(projection.type())
                    .attributes(items);
        }

        private AttributeConstructor select(SelectArray selected) {
            Collection<? extends Selected> items = selected.items();
            ArrayList<AttributeConstructor> list = new ArrayList<>(items.size());
            for (Selected item : items) {
                list.add(createObjectConstructor(item));
            }
            return new ArrayResult().items(list);
        }

        private AttributeConstructor select(SelectPrimitive selected) {
            BaseExpression expression = selected.expression();
            if (expandReferencePath && expression instanceof EntityPath) {
                return selectEntityPath(entityType, (EntityPath) expression);
            } else {
                return mewIndexableProperty(expression);
            }
        }

        private AttributeConstructor selectEntityPath(EntitySchema entityType, EntityPath path) {
            BasicAttribute property = entityType.getAttribute(path);
            if (property.isPrimitive()) {
                return indexable(property);
            } else if (property.isObject()) {
                return selectEntity((EntitySchema) property);
            } else {
                throw new UnsupportedOperationException();
            }
        }
    }


}
