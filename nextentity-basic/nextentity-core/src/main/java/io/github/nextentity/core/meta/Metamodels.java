package io.github.nextentity.core.meta;

import io.github.nextentity.core.expression.Attribute;
import io.github.nextentity.core.meta.graph.EntityProperty;
import io.github.nextentity.core.meta.graph.EntityReferenced;
import io.github.nextentity.core.meta.graph.EntitySchema;
import io.github.nextentity.core.meta.graph.Graph;
import io.github.nextentity.core.meta.graph.ProjectionProperty;
import io.github.nextentity.core.meta.graph.ProjectionReferenced;
import io.github.nextentity.core.meta.graph.ProjectionSchema;
import io.github.nextentity.core.meta.graph.Property;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import lombok.experimental.Accessors;
import lombok.experimental.Delegate;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Supplier;

public class Metamodels {

    @Getter
    @Accessors(fluent = true)
    public static class PropertyImpl<T extends Graph> implements Property {
        protected Class<?> javaType;
        protected String name;
        protected Method getter;
        protected Method setter;
        protected Field field;
        protected T declaringType;

        public PropertyImpl() {
        }

        public PropertyImpl(Class<?> javaType, String name, Method getter, Method setter, Field field, T declaringType) {
            this.javaType = javaType;
            this.name = name;
            this.getter = getter;
            this.setter = setter;
            this.field = field;
            this.declaringType = declaringType;
        }

        public PropertyImpl(Property property, T declaringType) {
            this(property.javaType(), property.name(), property.getter(), property.setter(), property.field(), declaringType);
        }

        @Override
        public String toString() {
            return name();
        }
    }


    @Getter
    @Accessors(fluent = true)
    public static class EntityPropertyImpl extends PropertyImpl<EntitySchema> implements EntityProperty {
        private final String columnName;
        private final boolean isVersion;
        @Getter(lazy = true)
        private final List<? extends EntityProperty> referencedAttributes = EntityProperty.super.referencedAttributes();

        public EntityPropertyImpl(Property property, String columnName, boolean isVersion) {
            super(property, (EntitySchema) property.declaringType());
            this.columnName = columnName;
            this.isVersion = isVersion;
        }

        @Override
        public EntityProperty get(String path) {
            throw new UnsupportedOperationException();
        }

        @Override
        public EntityProperty get(Attribute column) {
            throw new UnsupportedOperationException();
        }

    }


    @Getter
    @Setter
    @RequiredArgsConstructor
    @Accessors(fluent = true)
    public static class EntitySchemaImpl implements EntitySchema {

        private Class<?> javaType;
        private EntityProperty id;
        private EntityProperty version;
        private String tableName;
        private Map<String, EntityProperty> attributes;

        public Collection<EntityProperty> properties() {
            return attributes.values();
        }

        @Override
        public EntityProperty getProperty(String fieldName) {
            return attributes.get(fieldName);
        }

        @Override
        public String toString() {
            return javaType.getSimpleName();
        }
    }

    @Getter
    @Setter
    @RequiredArgsConstructor
    @Accessors(fluent = true)
    public static class SubSelectEntity extends EntitySchemaImpl implements SubSelectType {
        @Delegate
        private final EntitySchemaImpl entity;
        private final String subSelectSql;
    }

    @Getter
    @Setter
    @Accessors(fluent = true)
    public static class EntityReferencedImpl extends EntityPropertyImpl implements EntityReferenced {
        private String joinName;
        private String joinColumnName;
        private String referencedColumnName;
        private Supplier<EntitySchema> referencedSupplier;
        @Delegate(excludes = Graph.class)
        @Getter(lazy = true)
        private final EntitySchema referenced = referencedSupplier.get();

        public EntityReferencedImpl(Property property, String columnName) {
            super(property, columnName,false);
        }

        @Override
        public EntityProperty get(String path) {
            return referenced().getProperty(path);
        }

        @Override
        public EntityProperty get(Attribute column) {
            if (column instanceof EntityProperty) {
                return (EntityProperty) column;
            } else {
                EntityProperty cur = this;
                for (int i = 0; i < column.deep(); i++) {
                    cur = cur.get(cur.get(i));
                }
                return cur;
            }
        }
    }

    @Getter
    @Accessors(fluent = true)
    static class ProjectionAttributeImpl extends PropertyImpl<ProjectionSchema> implements ProjectionProperty {
        private final EntityProperty entityAttribute;

        ProjectionAttributeImpl(Property property, EntityProperty entityAttribute) {
            super(property, (ProjectionSchema) property.declaringType());
            this.entityAttribute = entityAttribute;
        }

    }

    @Getter
    @Accessors(fluent = true)
    static final class ProjectionReferencedImpl extends ProjectionAttributeImpl implements ProjectionReferenced {

        private final Function<ProjectionReferenced, Collection<? extends ProjectionProperty>> attributesSupplier;
        @Getter(lazy = true)
        private final Collection<? extends ProjectionProperty> properties = attributesSupplier.apply(this);

        ProjectionReferencedImpl(Property property,
                                 EntityReferenced entityAttribute,
                                 Function<ProjectionReferenced, Collection<? extends ProjectionProperty>> attributesSupplier) {
            super(property, entityAttribute);
            this.attributesSupplier = attributesSupplier;
        }

        @Override
        public EntityReferenced entityAttribute() {
            return (EntityReferenced) super.entityAttribute();
        }
    }

    @Getter
    @AllArgsConstructor
    @Accessors(fluent = true)
    static final class ProjectionSchemaImpl implements ProjectionSchema {
        private final Class<?> javaType;
        private final List<ProjectionProperty> properties;
        private final EntitySchema entityType;

        @Override
        public String toString() {
            return javaType.getSimpleName();
        }
    }
}
