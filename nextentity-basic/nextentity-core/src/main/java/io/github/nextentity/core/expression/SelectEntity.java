package io.github.nextentity.core.expression;

import io.github.nextentity.core.TypeCastUtil;
import io.github.nextentity.core.exception.BeanReflectiveException;
import io.github.nextentity.core.meta.graph.EntityProperty;
import io.github.nextentity.core.meta.graph.EntityReferenced;
import io.github.nextentity.core.meta.graph.EntitySchema;
import io.github.nextentity.core.meta.graph.Graph;
import io.github.nextentity.core.meta.graph.ProjectionProperty;
import io.github.nextentity.core.meta.graph.ProjectionReferenced;
import io.github.nextentity.core.meta.graph.ProjectionSchema;
import io.github.nextentity.core.meta.graph.Property;
import io.github.nextentity.core.meta.graph.Schema;
import io.github.nextentity.core.reflect.Arguments;
import io.github.nextentity.core.reflect.InstanceConstructor;
import io.github.nextentity.core.reflect.ReflectUtil;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.experimental.Accessors;
import org.jetbrains.annotations.NotNull;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.RecordComponent;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * @author HuangChengwei
 * @since 2024/4/17 下午3:21
 */
@Data
@Accessors(fluent = true)
public class SelectEntity implements SelectElement, InstanceConstructor {
    private static final Map<EntitySchema, SelectEntity> SELECT_ENTITIES = new HashMap<>();
    private static final Map<ProjectionSchema, SelectEntity> SELECT_PROJECTION = new HashMap<>();
    private final Schema schema;
    private final SchemaNode<? extends Schema> constructor;
    private final Collection<? extends EntityProperty> columns;
    private final HashSet<? extends EntityReferenced> fetch;

    public static SelectEntity of(EntitySchema entityType) {
        return SELECT_ENTITIES.computeIfAbsent(entityType, SelectEntity::new);
    }

    public static SelectEntity of(ProjectionSchema projection) {
        return SELECT_PROJECTION.computeIfAbsent(projection, SelectEntity::new);
    }

    private SelectEntity(EntitySchema entityType) {
        schema = entityType;
        Collection<? extends EntityProperty> attrs = entityType.properties();
        ArrayList<Node<? extends Property>> list = new ArrayList<>(attrs.size());
        ArrayList<EntityProperty> selectColumns = new ArrayList<>(attrs.size());
        for (EntityProperty attribute : attrs) {
            if (attribute.isBasic()) {
                Node<Property> attributeGraphProperty = new IndexedNode<>(attribute, selectColumns.size());
                selectColumns.add(attribute);
                list.add(attributeGraphProperty);
            }
        }
        constructor = new SchemaNode<>(entityType, list, true);
        columns = Collections.unmodifiableCollection(selectColumns);
        fetch = new HashSet<>();
    }

    private SelectEntity(ProjectionSchema entityType) {
        schema = entityType;
        ArrayList<EntityProperty> selectColumns = new ArrayList<>();
        constructor = getSchemaNode(entityType, selectColumns, true, true);
        columns = Collections.unmodifiableCollection(selectColumns);
        fetch = new HashSet<>();
    }

    private @NotNull SchemaNode<? extends Schema> getSchemaNode(ProjectionSchema entityType,
                                                                ArrayList<EntityProperty> selectColumns,
                                                                boolean root, boolean includeReferenced) {
        final SchemaNode<? extends Schema> constructor;
        Collection<? extends ProjectionProperty> attrs = entityType.properties();
        ArrayList<Node<? extends Property>> list = new ArrayList<>(attrs.size());
        for (ProjectionProperty attribute : attrs) {
            if (attribute.isBasic()) {
                Node<Property> attributeGraphProperty = new IndexedNode<>(attribute, selectColumns.size());
                selectColumns.add(attribute.entityAttribute());
                list.add(attributeGraphProperty);
            } else if (includeReferenced) {
                ProjectionReferenced referenced = (ProjectionReferenced) attribute;
                if (referenced.deep() > 16) {
                    continue;
                }
                Node<?> schemaNode = getSchemaNode(referenced, selectColumns, false, !referenced.circularReferenced());
                Node<? extends Property> node = TypeCastUtil.unsafeCast(schemaNode);
                list.add(node);
            }
        }
        constructor = new SchemaNode<>(entityType, list, root);
        return constructor;
    }

    private SelectEntity(Schema schema,
                         SchemaNode<? extends Schema> constructor,
                         Collection<? extends EntityProperty> columns,
                         HashSet<? extends EntityReferenced> fetch) {
        this.schema = schema;
        this.constructor = constructor;
        this.columns = columns;
        this.fetch = fetch;
    }

    public SelectEntity fetch(Collection<? extends EntityReferenced> fetchList) {
        SchemaNode<? extends Schema> result = constructor.copy();
        ArrayList<EntityProperty> selectColumns = new ArrayList<>(columns);
        HashSet<EntityReferenced> fetchSet = TypeCastUtil.unsafeCast(fetch.clone());
        for (EntityReferenced anyToOneAttribute : fetchList) {
            SchemaNode<? extends Schema> cur = result;
            List<EntityReferenced> attributes = TypeCastUtil.cast(anyToOneAttribute.referencedAttributes());
            for (EntityReferenced fetch : attributes) {
                if (fetchSet.contains(fetch)) {
                    for (Node<? extends Property> property : cur.properties()) {
                        if (Objects.equals(property.type().name(), fetch.name())) {
                            cur = (SchemaNode<? extends Schema>) property;
                            break;
                        }
                    }
                    continue;
                }
                fetchSet.add(fetch);
                ArrayList<Node<? extends Property>> graphNodes = new ArrayList<>();
                for (EntityProperty basic : fetch.properties()) {
                    if (basic.isBasic()) {
                        Node<Property> attributeGraphProperty = new IndexedNode<>(basic, selectColumns.size());
                        selectColumns.add(basic);
                        graphNodes.add(attributeGraphProperty);
                    }
                }
                SchemaNode<EntityReferenced> node = new SchemaNode<>(fetch, graphNodes, false);
                cur.properties.add(node);
                cur = node;
            }
        }
        return new SelectEntity(schema, result, selectColumns, fetchSet);
    }

    public Class<?> javaType() {
        return schema.javaType();
    }

    public Collection<? extends EntityProperty> columns() {
        return columns;
    }

    public Collection<? extends EntityReferenced> fetch() {
        return fetch;
    }

    @Override
    public Object newInstance(Arguments arguments) {
        return constructor.newInstance(arguments);
    }

    @Getter
    @EqualsAndHashCode(callSuper = true)
    private static class SchemaNode<T extends Schema> extends Node<T> {
        private final List<Node<? extends Property>> properties;
        private InstanceConstructor constructor;

        public SchemaNode(T type, List<Node<? extends Property>> properties, boolean root) {
            super(type);
            this.properties = properties;
        }

        @Override
        public SchemaNode<T> copy() {
            return new SchemaNode<>(type, properties.stream().map(Node::copy).collect(Collectors.toList()), false);
        }

        @Override
        public Object newInstance(Arguments arguments) {
            Class<?> javaType = type.javaType();
            if (constructor == null) {
                constructor = getConstructor(javaType);
            }
            return constructor.newInstance(arguments);
        }

        private InstanceConstructor getConstructor(Class<?> javaType) {
            if (javaType.isInterface()) {
                return new Interface();
            } else if (javaType.isRecord()) {
                return new Record();
            } else {
                return new Bean();
            }
        }

        class Bean implements InstanceConstructor {
            public Object newInstance(Arguments arguments) {
                Object result = null;
                for (Node<? extends Property> property : properties) {
                    Object value = property.newInstance(arguments);
                    if (value != null) {
                        if (result == null) {
                            result = ReflectUtil.newInstance(type.javaType());
                        }
                        property.type().set(result, value);
                    }
                }
                return result;
            }
        }

        class Interface implements InstanceConstructor {
            @Override
            public Object newInstance(Arguments arguments) {
                Map<Method, Object> map = new HashMap<>();
                boolean hasNonnullProperty = false;
                for (Node<? extends Property> property : properties) {
                    Object extract = property.newInstance(arguments);
                    hasNonnullProperty = hasNonnullProperty || extract != null;
                    map.put(property.type.getter(), extract);
                }
                if (hasNonnullProperty) {
                    return ReflectUtil.newProxyInstance(type.javaType(), map);
                } else {
                    return null;
                }
            }
        }

        class Record implements InstanceConstructor {
            public Class<?>[] parameterTypes;

            public Record() {
                Class<?> resultType = type.javaType();
                RecordComponent[] components = resultType.getRecordComponents();
                parameterTypes = new Class[components.length];
                Map<String, Integer> index = new HashMap<>();
                for (int i = 0; i < components.length; i++) {
                    RecordComponent component = components[i];
                    parameterTypes[i] = component.getType();
                    index.put(component.getName(), i);
                }
                properties.sort(Comparator.comparingInt(v -> index.get(v.type().name())));
            }

            @Override
            public Object newInstance(Arguments arguments) {
                try {
                    Class<?> resultType = type.javaType();
                    RecordComponent[] components = resultType.getRecordComponents();
                    Object[] args = new Object[components.length];
                    boolean hasNonnullProperty = false;
                    for (int i = 0; i < properties.size(); i++) {
                        Object extract = properties.get(i).newInstance(arguments);
                        hasNonnullProperty = hasNonnullProperty || extract != null;
                        args[i] = extract;
                    }
                    if (!hasNonnullProperty) {
                        return null;
                    }
                    Constructor<?> constructor = resultType.getDeclaredConstructor(parameterTypes);
                    try {
                        return constructor.newInstance(args);
                    } catch (Exception e) {
                        System.out.println(Arrays.toString(args));
                        throw e;
                    }
                } catch (ReflectiveOperationException e) {
                    throw new BeanReflectiveException(e);
                }
            }
        }


    }

    @Data
    private abstract static class Node<T extends Graph> implements InstanceConstructor {
        protected final T type;

        public Node<T> copy() {
            return this;
        }

    }

    @Getter
    @EqualsAndHashCode(callSuper = true)
    private static class IndexedNode<T extends Property> extends Node<T> {
        private final int index;

        public IndexedNode(T type, int index) {
            super(type);
            this.index = index;
        }

        @Override
        public Object newInstance(Arguments arguments) {
            return arguments.get(index);
        }
    }


}
