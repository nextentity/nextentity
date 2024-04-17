package io.github.nextentity.core.meta;

import io.github.nextentity.core.PathReference;
import io.github.nextentity.core.annotaion.EntityField;
import io.github.nextentity.core.annotaion.SubSelect;
import io.github.nextentity.core.exception.BeanReflectiveException;
import io.github.nextentity.core.meta.Metamodels.EntitySchemaImpl;
import io.github.nextentity.core.meta.Metamodels.EntityPropertyImpl;
import io.github.nextentity.core.meta.Metamodels.EntityReferencedImpl;
import io.github.nextentity.core.meta.Metamodels.ProjectionAttributeImpl;
import io.github.nextentity.core.meta.Metamodels.ProjectionReferencedImpl;
import io.github.nextentity.core.meta.Metamodels.PropertyImpl;
import io.github.nextentity.core.meta.Metamodels.ProjectionSchemaImpl;
import io.github.nextentity.core.meta.Metamodels.SubSelectEntity;
import io.github.nextentity.core.meta.graph.EntitySchema;
import io.github.nextentity.core.meta.graph.EntityProperty;
import io.github.nextentity.core.meta.graph.EntityReferenced;
import io.github.nextentity.core.meta.graph.Graph;
import io.github.nextentity.core.meta.graph.ProjectionSchema;
import io.github.nextentity.core.meta.graph.ProjectionProperty;
import io.github.nextentity.core.meta.graph.Property;
import io.github.nextentity.core.reflect.ReflectUtil;
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
import java.util.stream.Collectors;

@SuppressWarnings("PatternVariableCanBeUsed")
@Slf4j
public abstract class AbstractMetamodel implements Metamodel {

    private final Map<Class<?>, EntitySchema> entityTypes = new ConcurrentHashMap<>();
    private final Map<List<Class<?>>, ProjectionSchema> projections = new ConcurrentHashMap<>();

    @Override
    public EntitySchema getEntity(Class<?> entityType) {
        return entityTypes.computeIfAbsent(entityType, this::createEntityType);
    }

    @Override
    public ProjectionSchema getProjection(Class<?> entityType, Class<?> projectionType) {
        List<Class<?>> key = Arrays.asList(entityType, projectionType);
        return projections.computeIfAbsent(key, k -> createProjection(entityType, projectionType));
    }

    @NotNull
    protected ProjectionSchema createProjection(Class<?> baseType, Class<?> projectionType) {
        ArrayList<ProjectionProperty> list = new ArrayList<>();
        EntitySchema entity = getEntity(baseType);
        List<ProjectionProperty> immutable = Collections.unmodifiableList(list);
        ProjectionSchemaImpl result = new ProjectionSchemaImpl(projectionType, immutable, entity);
        ArrayList<ProjectionProperty> projectionProperties = getProjectionProperties(result);
        list.addAll(projectionProperties);
        list.trimToSize();
        return result;
    }

    protected ArrayList<ProjectionProperty> getProjectionProperties(ProjectionSchema projectionGraph) {
        ArrayList<ProjectionProperty> list = new ArrayList<>();
        List<Property> attributes = getProjectionAttributes(projectionGraph.javaType(), projectionGraph);
        EntitySchema entity = projectionGraph.entityType();
        for (Property attribute : attributes) {
            EntityProperty entityAttribute = getEntityAttribute(attribute, entity);
            if (entityAttribute == null) {
                continue;
            }
            if (entityAttribute instanceof EntityReferenced) {
                ProjectionReferencedImpl o = new ProjectionReferencedImpl(
                        attribute, (EntityReferenced) entityAttribute, this::getProjectionProperties);
                list.add(o);
            } else if (attribute.javaType() == entityAttribute.javaType()) {
                list.add(new ProjectionAttributeImpl(attribute, entityAttribute));
            }
        }
        return list;
    }

    // private void addProjectionAttributes(Class<?> projectionType,
    //                                      ProjectionGraph projectionGraph,
    //                                      EntityGraph entity,
    //                                      ArrayList<ProjectionProperty> list,
    //                                      int deep,
    //                                      int maxDeep) {
    //     if (deep == maxDeep) {
    //         return;
    //     }
    //     List<Property> attributes = getProjectionAttributes(projectionType, projectionGraph);
    //     for (Property attribute : attributes) {
    //         EntityProperty entityAttribute = getEntityAttribute(attribute, entity);
    //         if (entityAttribute == null) {
    //             continue;
    //         }
    //         ProjectionAttributeImpl projectionAttribute = new ProjectionAttributeImpl(attribute, entityAttribute);
    //         if (entityAttribute instanceof EntityGraph) {
    //             ProjectionReferencedImpl o = new ProjectionReferencedImpl(projectionAttribute, projectionGraph, entity);
    //             addProjectionAttributes(attribute.javaType(), o,
    //                     (EntityGraph) entityAttribute, list, deep + 1, maxDeep);
    //         } else if (attribute.javaType() == entityAttribute.javaType()) {
    //             list.add(projectionAttribute);
    //         }
    //     }
    // }

    private List<Property> getProjectionAttributes(Class<?> projectionType, Graph owner) {
        if (projectionType.isInterface()) {
            return getInterfaceAttributes(projectionType, owner);
        } else if (projectionType.isRecord()) {
            return getRecordAttributes(projectionType, owner);
        }
        return getBeanAttributes(projectionType, owner);
    }

    private List<Property> getRecordAttributes(Class<?> projectionType, Graph owner) {
        RecordComponent[] components = projectionType.getRecordComponents();
        return Arrays.stream(components)
                .map(it -> newAttribute(null, it.getAccessor(), null, owner))
                .collect(Collectors.toList());
    }

    protected EntityProperty getEntityAttribute(Property attribute, EntitySchema entity) {
        EntityProperty entityAttribute = getEntityAttributeByAnnotation(attribute, entity);
        return entityAttribute == null
                ? entity.getProperty(attribute.name())
                : entityAttribute;
    }

    private EntityProperty getEntityAttributeByAnnotation(Property attribute, EntitySchema entity) {
        EntityField entityAttribute = getAnnotation(attribute, EntityField.class);
        if (entityAttribute == null || entityAttribute.value().isEmpty()) {
            return null;
        }
        String value = entityAttribute.value();
        String[] split = value.split("\\.");
        Graph cur = entity;
        for (String s : split) {
            if (cur instanceof EntitySchema) {
                cur = ((EntitySchema) cur).getProperty(s);
            } else {
                throw new IllegalStateException("entity attribute " + value + " not exist");
            }
        }
        if (cur instanceof EntityProperty) {
            if (attribute.javaType() != cur.javaType()) {
                throw new IllegalStateException("entity attribute " + value + " type mismatch");
            }
            return (EntityProperty) cur;
        } else {
            throw new IllegalStateException("entity attribute " + value + " not exist");
        }
    }

    @NotNull
    private List<Property> getInterfaceAttributes(Class<?> clazz, Graph owner) {
        return Arrays.stream(clazz.getMethods())
                .map(it -> newAttribute(null, it, null, owner))
                .collect(Collectors.toList());
    }

    protected abstract String getTableName(Class<?> javaType);

    protected abstract boolean isMarkedId(Property attribute);

    protected abstract String getReferencedColumnName(Property attribute);

    protected abstract String getJoinColumnName(Property attribute);

    protected abstract boolean isVersionField(Property attribute);

    protected abstract boolean isTransient(Property attribute);

    protected abstract boolean isBasicField(Property attribute);

    protected abstract boolean isAnyToOne(Property attribute);

    protected abstract String getColumnName(Property attribute);

    protected abstract Field[] getSuperClassField(Class<?> baseClass, Class<?> superClass);

    protected EntitySchema createEntityType(Class<?> entityType) {
        EntitySchemaImpl result = new EntitySchemaImpl();
        createEntityType(entityType, result, result);
        SubSelect[] type = entityType.getAnnotationsByType(SubSelect.class);
        if (type.length == 1) {
            return new SubSelectEntity(result, type[0].value());
        }
        return result;
    }

    protected EntitySchemaImpl createEntityType(Class<?> entityType, EntitySchemaImpl result, Graph owner) {
        result.javaType(entityType);
        Map<String, EntityProperty> map = new LinkedHashMap<>();
        result.attributes(Collections.unmodifiableMap(map));
        result.tableName(getTableName(entityType));
        List<Property> attributes = getBeanAttributes(entityType, owner);
        boolean hasVersion = false;
        for (Property attr : attributes) {
            if (map.containsKey(attr.name())) {
                throw new IllegalStateException("Duplicate key");
            }
            if (isTransient(attr)) {
                continue;
            }

            EntityProperty attribute;
            if (isBasicField(attr)) {
                boolean versionColumn = false;
                if (isVersionField(attr)) {
                    if (hasVersion) {
                        log.warn("duplicate attributes: {}, ignored", attr.name());
                    } else {
                        versionColumn = hasVersion = true;
                    }
                }
                attribute = new EntityPropertyImpl(attr, getColumnName(attr), versionColumn);
                if (versionColumn) {
                    result.version(attribute);
                }
            } else if (isAnyToOne(attr)) {
                EntityReferencedImpl ato = new EntityReferencedImpl(attr, getColumnName(attr));
                ato.joinName(getJoinColumnName(attr));
                ato.referencedColumnName(getReferencedColumnName(attr));
                ato.referencedSupplier(() -> createEntityType(attr.javaType(), new EntitySchemaImpl(), ato));
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

    protected void setAnyToOneAttributeColumnName(Map<String, EntityProperty> map) {
        for (Entry<String, EntityProperty> entry : map.entrySet()) {
            EntityProperty value = entry.getValue();
            if (value instanceof EntityReferencedImpl) {
                EntityReferencedImpl attr = (EntityReferencedImpl) value;
                String joinColumnName = getJoinColumnName(map, attr);
                attr.joinColumnName(joinColumnName);
            }
        }
    }

    protected String getJoinColumnName(Map<String, EntityProperty> map, EntityReferencedImpl attr) {
        String joinName = attr.joinName();
        EntityProperty join = map.get(joinName);
        return join.isBasic()
                ? (join).columnName()
                : joinName;
    }

    protected List<Property> getBeanAttributes(Class<?> type, Graph owner) {
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
        List<Property> attributes = getDeclaredFields(type).stream()
                .map(field -> newAttribute(owner, field, map.remove(field.getName())))
                .collect(Collectors.toList());
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

    private Property newAttribute(Graph owner, Field field, PropertyDescriptor descriptor) {
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

    protected <T extends Graph> Property newAttribute(Field field, Method getter, Method setter, T owner) {
        Class<?> javaType = getter != null ? getter.getReturnType() : field.getType();
        String name = field != null ? field.getName() : PathReference.getPropertyName(getter.getName());
        return new PropertyImpl<>(javaType, name, getter, setter, field, owner);
    }

    protected <T extends Annotation> T getAnnotation(Property attribute, Class<T> annotationClass) {
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
