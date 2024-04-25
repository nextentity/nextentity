package io.github.nextentity.core.meta;

import io.github.nextentity.core.BasicExpressions;
import io.github.nextentity.core.api.expression.EntityPath;
import io.github.nextentity.core.api.expression.QueryStructure.Selected.SelectEntity;
import io.github.nextentity.core.api.expression.QueryStructure.Selected.SelectProjection;
import io.github.nextentity.core.reflect.SelectedConstruct;
import io.github.nextentity.core.reflect.schema.Attribute;
import io.github.nextentity.core.reflect.schema.Schema;
import io.github.nextentity.core.util.ImmutableList;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;
import lombok.experimental.Delegate;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.stream.Collectors;

public class Metamodels {

    @Getter
    @Accessors(fluent = true)
    public static class AttributeImpl<T extends Schema> implements Attribute {
        protected Class<?> type;
        protected String name;
        protected Method getter;
        protected Method setter;
        protected Field field;
        protected T declareBy;

        public AttributeImpl() {
        }

        public AttributeImpl(Class<?> javaType, String name, Method getter, Method setter, Field field, T declareBy) {
            this.type = javaType;
            this.name = name;
            this.getter = getter;
            this.setter = setter;
            this.field = field;
            this.declareBy = declareBy;
        }

        public AttributeImpl(Attribute attribute, T declareBy) {
            this(attribute.type(), attribute.name(), attribute.getter(), attribute.setter(), attribute.field(), declareBy);
        }

        @Override
        public String toString() {
            return name();
        }
    }


    @Getter
    @Accessors(fluent = true)
    public static class BasicAttributeImpl extends AttributeImpl<EntitySchema> implements BasicAttribute {
        protected String columnName;
        private final boolean isVersion;
        private final List<? extends BasicAttribute> referencedAttributes;
        private final EntityPath path;
        private DatabaseType databaseType;

        public BasicAttributeImpl(Attribute attribute, String columnName, boolean isVersion) {
            super(attribute, (EntitySchema) attribute.declareBy());
            this.columnName = columnName;
            this.isVersion = isVersion;
            this.referencedAttributes = BasicAttribute.super.attributePaths();
            List<String> paths = referencedAttributes.stream()
                    .map(BasicAttribute::name)
                    .collect(ImmutableList.collector(referencedAttributes.size()));
            this.path = BasicExpressions.column(paths);
        }

        public void databaseType(DatabaseType converter) {
            this.databaseType = converter == null ? new IdentityDatabaseType() : converter;
        }

        class IdentityDatabaseType implements DatabaseType {
            @Override
            public Class<?> databaseType() {
                return type();
            }

            @Override
            public Object toDatabaseType(Object value) {
                return value;
            }

            @Override
            public Object toAttributeType(Object value) {
                return value;
            }

        }

    }

    @Getter
    @Setter
    @Accessors(fluent = true)
    public static class EntityTypeImpl extends EntitySchemaImpl implements EntityType {
        private BiFunction<EntityType, Class<?>, ProjectionType> projectionTypes;
        private volatile SelectedConstruct constructor;

        @Override
        public ProjectionType getProjection(Class<?> type) {
            return projectionTypes.apply(this, type);
        }

        @Override
        public SelectedConstruct constructor() {
            if (constructor == null) {
                synchronized (this) {
                    if (constructor == null) {
                        SelectEntity selected = new SelectEntity()
                                .type(type())
                                .fetch(Collections.emptyList());
                        constructor = SelectedConstruct.of(this, selected, false);
                    }
                }
            }
            return constructor;
        }

        public EntityTypeImpl() {
        }


    }

    @Getter
    @Setter
    @Accessors(fluent = true)
    public static class EntitySchemaImpl implements EntitySchema {

        private Class<?> type;
        private BasicAttribute id;
        private BasicAttribute version;
        private String tableName;
        private Map<String, BasicAttribute> dictionary;
        private transient List<? extends BasicAttribute> primitiveAttributes;

        public EntitySchemaImpl() {
        }

        public Collection<BasicAttribute> attributes() {
            return dictionary.values();
        }

        @Override
        public BasicAttribute getAttribute(String fieldName) {
            return dictionary.get(fieldName);
        }

        @Override
        public String toString() {
            return type.getSimpleName();
        }

        @Override
        public String name() {
            return type.getSimpleName();
        }

        @Override
        public Schema declareBy() {
            return null;
        }

        @Override
        public List<? extends BasicAttribute> primitiveAttributes() {
            if (primitiveAttributes == null) {
                primitiveAttributes = EntitySchema.super.primitiveAttributes();
            }
            return primitiveAttributes;
        }
    }

    @Getter
    @Setter
    @Accessors(fluent = true)
    public static class SubSelectEntity extends EntityTypeImpl implements SubSelectType {
        private final String subSelectSql;

        public SubSelectEntity(String subSelectSql) {
            this.subSelectSql = subSelectSql;
        }
    }

    @Getter
    @Setter
    @Accessors(fluent = true)
    public static class AssociationAttributeImpl extends BasicAttributeImpl implements AssociationAttribute {
        private String joinName;
        private String referencedColumnName;
        private Supplier<EntitySchema> referencedSupplier;
        @Delegate(excludes = Schema.class)
        @Getter(lazy = true)
        private final EntitySchema referenced = referencedSupplier.get();

        public AssociationAttributeImpl(Attribute attribute, String columnName) {
            super(attribute, columnName, false);
            databaseType(new IdentityDatabaseType());
        }

        public void columnName(String columnName) {
            this.columnName = columnName;
        }

    }

    @Getter
    @Accessors(fluent = true)
    static class ProjectionAttributeImpl extends AttributeImpl<ProjectionType> implements ProjectionBasicAttribute {
        private final BasicAttribute entityAttribute;

        ProjectionAttributeImpl(Attribute attribute, BasicAttribute entityAttribute) {
            super(attribute, (ProjectionType) attribute.declareBy());
            this.entityAttribute = entityAttribute;
        }

    }

    @Getter
    @Accessors(fluent = true)
    static final class ProjectionAssociationAttributeImpl extends ProjectionAttributeImpl implements ProjectionAssociationAttribute {

        private final Function<ProjectionAssociationAttribute, Collection<? extends ProjectionBasicAttribute>> attributesSupplier;
        @Getter(lazy = true)
        private final Map<String, ? extends ProjectionBasicAttribute> dictionary = attributesSupplier.apply(this)
                .stream().collect(Collectors.toMap(Attribute::name, Function.identity()));


        ProjectionAssociationAttributeImpl(Attribute attribute,
                                           AssociationAttribute entityAttribute,
                                           Function<ProjectionAssociationAttribute, Collection<? extends ProjectionBasicAttribute>> attributesSupplier) {
            super(attribute, entityAttribute);
            this.attributesSupplier = attributesSupplier;
        }

        @Override
        public AssociationAttribute entityAttribute() {
            return (AssociationAttribute) super.entityAttribute();
        }


        @Override
        public Collection<? extends ProjectionBasicAttribute> attributes() {
            return dictionary().values();
        }

        @Override
        public SelectedConstruct constructor() {
            throw new UnsupportedOperationException();
        }

        @Override
        public Attribute getAttribute(String name) {
            return dictionary().get(name);
        }
    }

    @Getter
    @Accessors(fluent = true)
    static final class ProjectionSchemaImpl implements ProjectionType {
        private final Class<?> type;
        private final Map<String, ProjectionBasicAttribute> dictionary = new HashMap<>();
        private final EntitySchema entityType;
        private volatile SelectedConstruct constructor;

        public ProjectionSchemaImpl(Class<?> type, EntitySchema entityType) {
            this.type = type;
            this.entityType = entityType;
        }

        public void setProperties(List<ProjectionBasicAttribute> properties) {
            this.dictionary.putAll(properties.stream().collect(Collectors.toMap(Attribute::name, Function.identity())));
        }


        @Override
        public String toString() {
            return name();
        }

        @Override
        public Collection<? extends ProjectionBasicAttribute> attributes() {
            return dictionary.values();
        }

        @Override
        public SelectedConstruct constructor() {
            if (constructor == null) {
                synchronized (this) {
                    if (constructor == null) {
                        SelectProjection selected = new SelectProjection()
                                .entityType(entityType.type())
                                .type(type());
                        constructor = SelectedConstruct.of((EntityType) entityType(), selected, false);
                    }
                }
            }
            return constructor;
        }

        @Override
        public ProjectionBasicAttribute getAttribute(String name) {
            return dictionary.get(name);
        }

        @Override
        public String name() {
            return type.getSimpleName();
        }

        @Override
        public Attribute declareBy() {
            return null;
        }
    }
}
