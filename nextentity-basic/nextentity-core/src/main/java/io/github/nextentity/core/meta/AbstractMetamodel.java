package io.github.nextentity.core.meta;

import io.github.nextentity.core.PathReference;
import io.github.nextentity.core.annotaion.EntityAttribute;
import io.github.nextentity.core.annotaion.SubSelect;
import io.github.nextentity.core.exception.BeanReflectiveException;
import io.github.nextentity.core.meta.Metamodels.AssociationAttributeImpl;
import io.github.nextentity.core.meta.Metamodels.AttributeImpl;
import io.github.nextentity.core.meta.Metamodels.BasicAttributeImpl;
import io.github.nextentity.core.meta.Metamodels.EntitySchemaImpl;
import io.github.nextentity.core.meta.Metamodels.EntityTypeImpl;
import io.github.nextentity.core.meta.Metamodels.ProjectionAssociationAttributeImpl;
import io.github.nextentity.core.meta.Metamodels.ProjectionAttributeImpl;
import io.github.nextentity.core.meta.Metamodels.ProjectionSchemaImpl;
import io.github.nextentity.core.meta.Metamodels.SubSelectEntity;
import io.github.nextentity.core.reflect.ReflectUtil;
import io.github.nextentity.core.reflect.schema.Attribute;
import io.github.nextentity.core.reflect.schema.Schema;
import io.github.nextentity.core.util.ImmutableList;
import lombok.extern.slf4j.Slf4j;
import org.jetbrains.annotations.NotNull;

import java.beans.BeanInfo;
import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.RecordComponent;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;

@Slf4j
public abstract class AbstractMetamodel implements Metamodel {

    private final Map<Class<?>, EntityType> entityTypes = new ConcurrentHashMap<>();
    private final Map<List<Class<?>>, ProjectionType> projections = new ConcurrentHashMap<>();

    @Override
    public EntityType getEntity(Class<?> entityType) {
        return entityTypes.computeIfAbsent(entityType, this::createEntityType);
    }

    protected ProjectionType getProjection(Class<?> entityType, Class<?> projectionType) {
        List<Class<?>> key = Arrays.asList(entityType, projectionType);
        return projections.computeIfAbsent(key, k -> createProjection(entityType, projectionType));
    }

    @NotNull
    protected ProjectionType createProjection(Class<?> baseType, Class<?> projectionType) {
        EntitySchema entity = getEntity(baseType);
        ProjectionSchemaImpl result = new ProjectionSchemaImpl(projectionType, entity);
        ArrayList<ProjectionBasicAttribute> projectionProperties = getProjectionProperties(result);
        result.setProperties(projectionProperties);
        return result;
    }

    protected ArrayList<ProjectionBasicAttribute> getProjectionProperties(ProjectionType projectionGraph) {
        ArrayList<ProjectionBasicAttribute> list = new ArrayList<>();
        List<Attribute> attributes = getProjectionAttributes(projectionGraph.type(), projectionGraph);
        EntitySchema entity = projectionGraph.entityType();
        for (Attribute attribute : attributes) {
            BasicAttribute entityAttribute = getEntityAttribute(attribute, entity);
            if (entityAttribute == null) {
                continue;
            }
            if (entityAttribute instanceof AssociationAttribute) {
                ProjectionAssociationAttributeImpl o = new ProjectionAssociationAttributeImpl(
                        attribute, (AssociationAttribute) entityAttribute, this::getProjectionProperties);
                list.add(o);
            } else if (attribute.type() == entityAttribute.type()) {
                list.add(new ProjectionAttributeImpl(attribute, entityAttribute));
            }
        }
        return list;
    }

    private List<Attribute> getProjectionAttributes(Class<?> projectionType, Schema owner) {
        if (projectionType.isInterface()) {
            return getInterfaceAttributes(projectionType, owner);
        } else if (projectionType.isRecord()) {
            return getRecordAttributes(projectionType, owner);
        }
        return getBeanAttributes(projectionType, owner);
    }

    private List<Attribute> getRecordAttributes(Class<?> projectionType, Schema owner) {
        RecordComponent[] components = projectionType.getRecordComponents();
        return Arrays.stream(components)
                .map(it -> newAttribute(null, it.getAccessor(), null, owner))
                .collect(ImmutableList.collector(components.length));
    }

    protected BasicAttribute getEntityAttribute(Attribute attribute, EntitySchema entity) {
        BasicAttribute entityAttribute = getEntityAttributeByAnnotation(attribute, entity);
        return entityAttribute == null
                ? entity.getAttribute(attribute.name())
                : entityAttribute;
    }

    private BasicAttribute getEntityAttributeByAnnotation(Attribute attribute, EntitySchema entity) {
        EntityAttribute entityAttribute = getAnnotation(attribute, EntityAttribute.class);
        if (entityAttribute == null || entityAttribute.value().isEmpty()) {
            return null;
        }
        String value = entityAttribute.value();
        String[] split = value.split("\\.");
        Schema cur = entity;
        for (String s : split) {
            if (cur instanceof EntitySchema) {
                cur = ((EntitySchema) cur).getAttribute(s);
            } else {
                throw new IllegalStateException("entity attribute " + value + " not exist");
            }
        }
        if (cur instanceof BasicAttribute) {
            if (attribute.type() != cur.type()) {
                throw new IllegalStateException("entity attribute " + value + " type mismatch");
            }
            return (BasicAttribute) cur;
        } else {
            throw new IllegalStateException("entity attribute " + value + " not exist");
        }
    }

    @NotNull
    private List<Attribute> getInterfaceAttributes(Class<?> clazz, Schema owner) {
        Method[] methods = clazz.getMethods();
        return Arrays.stream(methods)
                .map(it -> newAttribute(null, it, null, owner))
                .collect(ImmutableList.collector(methods.length));
    }

    protected abstract String getTableName(Class<?> javaType);

    protected abstract boolean isMarkedId(Attribute attribute);

    protected abstract String getReferencedColumnName(Attribute attribute);

    protected abstract String getJoinColumnName(Attribute attribute);

    protected abstract boolean isVersionField(Attribute attribute);

    protected abstract boolean isTransient(Attribute attribute);

    protected abstract boolean isBasicField(Attribute attribute);

    protected abstract boolean isAnyToOne(Attribute attribute);

    protected abstract String getColumnName(Attribute attribute);

    protected abstract Field[] getSuperClassField(Class<?> baseClass, Class<?> superClass);

    protected DatabaseType databaseType(Attribute attribute) {
        if (attribute.type().isEnum()) {
            return new OrdinalOfEnumType(attribute.type());
        }
        return null;
    }

    protected EntityType createEntityType(Class<?> entityType) {
        EntityTypeImpl result;
        SubSelect[] type = entityType.getAnnotationsByType(SubSelect.class);
        if (type.length == 1) {
            result = new SubSelectEntity(type[0].value());
        } else {
            result = new EntityTypeImpl();
        }
        createEntityType(entityType, result, result);
        result.projectionTypes((entity, projectionType) -> getProjection(entity.type(), projectionType));
        return result;
    }

    protected EntitySchemaImpl createEntityType(Class<?> entityType, EntitySchemaImpl result, Schema owner) {
        result.type(entityType);
        Map<String, BasicAttribute> map = new LinkedHashMap<>();
        result.dictionary(Collections.unmodifiableMap(map));
        result.tableName(getTableName(entityType));
        List<Attribute> attributes = getBeanAttributes(entityType, owner);
        boolean hasVersion = false;
        for (Attribute attr : attributes) {
            if (map.containsKey(attr.name())) {
                throw new IllegalStateException("Duplicate key");
            }
            if (isTransient(attr)) {
                continue;
            }

            BasicAttribute attribute;
            if (isBasicField(attr)) {
                boolean versionColumn = false;
                if (isVersionField(attr)) {
                    if (hasVersion) {
                        log.warn("duplicate attributes: {}, ignored", attr.name());
                    } else {
                        versionColumn = hasVersion = true;
                    }
                }
                BasicAttributeImpl impl = new BasicAttributeImpl(attr, getColumnName(attr), versionColumn);
                impl.databaseType(databaseType(attr));
                attribute = impl;
                if (versionColumn) {
                    result.version(attribute);
                }
            } else if (isAnyToOne(attr)) {
                AssociationAttributeImpl ato = new AssociationAttributeImpl(attr, getColumnName(attr));
                ato.joinName(getJoinColumnName(attr));
                ato.referencedColumnName(getReferencedColumnName(attr));
                ato.referencedSupplier(() -> createEntityType(attr.type(), new EntitySchemaImpl(), ato));
                attribute = ato;
            } else {
                log.warn("ignored attribute {}", attr.field());
                continue;
            }

            boolean isMarkedId = isMarkedId(attribute);
            if (isMarkedId || result.id() == null && "id".equals(attr.name())) {
                result.id(attribute);
            }
            map.put(attribute.name(), attribute);
        }
        setAnyToOneAttributeColumnName(map);
        return result;
    }

    protected void setAnyToOneAttributeColumnName(Map<String, BasicAttribute> map) {
        for (Entry<String, BasicAttribute> entry : map.entrySet()) {
            BasicAttribute value = entry.getValue();
            if (value instanceof AssociationAttributeImpl attr) {
                String joinColumnName = getJoinColumnName(map, attr);
                attr.columnName(joinColumnName);
            }
        }
    }

    protected String getJoinColumnName(Map<String, BasicAttribute> map, AssociationAttributeImpl attr) {
        String joinName = attr.joinName();
        BasicAttribute join = map.get(joinName);
        return join.isPrimitive()
                ? (join).columnName()
                : joinName;
    }

    protected List<Attribute> getBeanAttributes(Class<?> type, Schema owner) {
        Map<String, PropertyDescriptor> map = new HashMap<>();
        try {
            BeanInfo beanInfo = Introspector.getBeanInfo(type);
            PropertyDescriptor[] propertyDescriptors = beanInfo.getPropertyDescriptors();
            for (PropertyDescriptor descriptor : propertyDescriptors) {
                Field field = ReflectUtil.getDeclaredField(type, descriptor.getName());
                if (field != null) {
                    map.put(field.getName(), descriptor);
                }
            }
        } catch (IntrospectionException e) {
            throw new BeanReflectiveException(e);
        }
        Collection<Field> declaredFields = getDeclaredFields(type);
        List<Attribute> attributes = declaredFields.stream()
                .map(field -> newAttribute(owner, field, map.remove(field.getName())))
                .collect(ImmutableList.collector(declaredFields.size()));
        map.values().stream()
                .map(descriptor -> newAttribute(owner, null, descriptor))
                .forEach(attributes::add);
        return attributes;
    }

    protected Collection<Field> getDeclaredFields(Class<?> clazz) {
        Map<String, Field> map = new LinkedHashMap<>();
        Field[] fields = clazz.getDeclaredFields();
        putFieldsIfAbsent(map, fields);
        getSuperClassDeclaredFields(clazz, clazz.getSuperclass(), map);
        return map.values();
    }

    protected void putFieldsIfAbsent(Map<String, Field> map, Field[] fields) {
        for (Field field : fields) {
            if (filterDeclaredField(field)) {
                map.putIfAbsent(field.getName(), field);
            }
        }
    }

    protected void getSuperClassDeclaredFields(Class<?> baseClass, Class<?> clazz, Map<String, Field> map) {
        if (clazz == null) {
            return;
        }
        Field[] superClassField = getSuperClassField(baseClass, clazz);
        if (superClassField != null) {
            putFieldsIfAbsent(map, superClassField);
        }
        Class<?> superclass = clazz.getSuperclass();
        getSuperClassDeclaredFields(baseClass, superclass, map);
    }

    private Attribute newAttribute(Schema owner, Field field, PropertyDescriptor descriptor) {
        Method getter, setter;
        if (descriptor != null) {
            getter = descriptor.getReadMethod();
            setter = descriptor.getWriteMethod();
        } else {
            getter = setter = null;
        }
        return newAttribute(field, getter, setter, owner);
    }

    protected boolean filterDeclaredField(@NotNull Field field) {
        int modifiers = field.getModifiers();
        return !Modifier.isStatic(modifiers) && !Modifier.isTransient(modifiers) && !Modifier.isFinal(modifiers);
    }

    protected <T extends Schema> Attribute newAttribute(Field field, Method getter, Method setter, T owner) {
        Class<?> javaType = getter != null ? getter.getReturnType() : field.getType();
        String name = field != null ? field.getName() : PathReference.getFieldName(getter.getName());
        return new AttributeImpl<>(javaType, name, getter, setter, field, owner);
    }

    protected <T extends Annotation> T getAnnotation(Attribute attribute, Class<T> annotationClass) {
        T column = null;
        if (attribute.field() != null) {
            column = attribute.field().getAnnotation(annotationClass);
        }
        if (column == null) {
            Method getter = attribute.getter();
            if (getter != null) {
                column = getter.getAnnotation(annotationClass);
            }
        }
        return column;
    }

}
