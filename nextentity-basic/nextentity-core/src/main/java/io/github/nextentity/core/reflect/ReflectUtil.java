package io.github.nextentity.core.reflect;

import io.github.nextentity.core.exception.BeanReflectiveException;
import io.github.nextentity.core.meta.Attribute;
import io.github.nextentity.core.meta.ObjectType;
import io.github.nextentity.core.meta.Type;
import io.github.nextentity.core.util.Exceptions;
import lombok.SneakyThrows;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.beans.BeanInfo;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodHandles.Lookup;
import java.lang.invoke.MethodType;
import java.lang.reflect.AccessibleObject;
import java.lang.reflect.Array;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

public class ReflectUtil {
    private static final Map<Class<?>, Object> SINGLE_ENUM_MAP = new ConcurrentHashMap<>();

    static final Map<Collection<? extends Attribute>, ObjectConstructor> CONSTRUCTORS = new ConcurrentHashMap<>();

    @Nullable
    public static Field getDeclaredField(@NotNull Class<?> clazz, String name) {
        try {
            return clazz.getDeclaredField(name);
        } catch (NoSuchFieldException e) {
            Class<?> superclass = clazz.getSuperclass();
            if (superclass != null) {
                return getDeclaredField(superclass, name);
            }
        }
        return null;
    }

    @SneakyThrows
    public static <T> void copyTargetNullFields(T src, T target, Class<T> type) {
        BeanInfo beanInfo = Introspector.getBeanInfo(type);
        PropertyDescriptor[] descriptors = beanInfo.getPropertyDescriptors();
        for (PropertyDescriptor descriptor : descriptors) {
            Method reader = descriptor.getReadMethod();
            Method writer = descriptor.getWriteMethod();
            if (reader != null && writer != null) {
                Object tv = reader.invoke(target);
                if (tv != null) {
                    continue;
                }
                Object sv = reader.invoke(src);
                if (sv != null) {
                    writer.invoke(target, sv);
                }
            }
        }
    }

    public static InstanceConstructor getRowInstanceConstructor(Collection<? extends Attribute> attributes, Class<?> resultType) {
        ObjectConstructor schema = CONSTRUCTORS.computeIfAbsent(attributes, ReflectUtil::doGetConstructor);
        if (schema.type.javaType() != resultType) {
            throw new IllegalArgumentException();
        }
        return schema;
    }

    private static ObjectConstructor doGetConstructor(Collection<? extends Attribute> attributes) {
        Map<Type, Property> map = new HashMap<>();
        ObjectConstructor result = null;
        for (Attribute attribute : attributes) {
            Type cur = attribute;
            while (true) {
                Property property = map.computeIfAbsent(cur, ReflectUtil::newProperty);
                cur = Attribute.getDeclaringType(cur);
                if (cur == null) {
                    if (result == null) {
                        result = (ObjectConstructor) property;
                    } else if (result != property) {
                        throw new IllegalArgumentException();
                    }
                    break;
                }
            }
        }
        if (result == null) {
            throw new IllegalArgumentException();
        }
        int i = 0;
        for (Attribute attribute : attributes) {
            PropertyImpl property = (PropertyImpl) map.get(attribute);
            property.setIndex(i++);
        }
        Map<Type, List<Entry<Type, Property>>> attrs = map.entrySet().stream()
                .filter(it -> Attribute.getDeclaringType(it.getKey()) != null)
                .collect(Collectors.groupingBy(e -> Attribute.getDeclaringType(e.getKey())));
        for (Entry<Type, List<Entry<Type, Property>>> entry : attrs.entrySet()) {
            Property property = map.get(entry.getKey());
            List<Entry<Type, Property>> v = entry.getValue();
            if (v != null && !v.isEmpty()) {
                ((ObjectConstructor) property).setProperties(v.stream()
                        .map(Entry::getValue)
                        .toArray(Property[]::new));
            }
        }
        result.root = true;
        return result;
    }

    private static Property newProperty(Type type) {
        if (type instanceof ObjectType) {
            Class<?> javaType = type.javaType();
            if (javaType.isInterface()) {
                return new InterfaceConstructor(type);
            } else {
                return new BeanConstructor(type);
            }
        } else {
            return new PropertyImpl((Attribute) type);
        }
    }

    @NotNull
    public static Object newInstance(Class<?> resultType) {
        try {
            return resultType.getConstructor().newInstance();
        } catch (ReflectiveOperationException e) {
            throw new BeanReflectiveException(e);
        }
    }

    public static Object invokeDefaultMethod(Object proxy, Method method, Object[] args) throws Throwable {
        final float version = Float.parseFloat(System.getProperty("java.class.version"));
        if (version <= 52) {
            final Constructor<Lookup> constructor = MethodHandles.Lookup.class
                    .getDeclaredConstructor(Class.class);
            constructor.setAccessible(true);

            final Class<?> clazz = method.getDeclaringClass();
            MethodHandles.Lookup lookup = constructor.newInstance(clazz);
            return lookup
                    .in(clazz)
                    .unreflectSpecial(method, clazz)
                    .bindTo(proxy)
                    .invokeWithArguments(args);
        } else {
            return MethodHandles.lookup()
                    .findSpecial(
                            method.getDeclaringClass(),
                            method.getName(),
                            MethodType.methodType(method.getReturnType(), new Class[0]),
                            method.getDeclaringClass()
                    )
                    .bindTo(proxy)
                    .invokeWithArguments(args);
        }
    }

    public static Object getFieldValue(Field field, Object instance) throws IllegalAccessException {
        checkAccessible(field, instance);
        return field.get(instance);
    }

    public static void setFieldValue(Field field, Object instance, Object value) throws IllegalAccessException {
        checkAccessible(field, instance);
        field.set(instance, value);
    }

    private static void checkAccessible(AccessibleObject accessible, Object instance) {
        if (!isAccessible(accessible, instance)) {
            accessible.setAccessible(true);
        }
    }

    public static boolean isAccessible(AccessibleObject accessibleObject, Object instance) {
        return accessibleObject.isAccessible();
    }

    @NotNull
    public static Object newProxyInstance(Property[] fields, @NotNull Class<?> resultType, Map<Method, Object> map) {
        ClassLoader classLoader = resultType.getClassLoader();
        Class<?>[] interfaces = {resultType};
        return Proxy.newProxyInstance(classLoader, interfaces, new InstanceInvocationHandler(fields, resultType, map));
    }

    public static void typeCheck(Object value, Class<?> type) {
        if (value == null) {
            if (type.isPrimitive()) {
                throw new BeanReflectiveException("primitive type value can not be null");
            }
        } else if (!type.isInstance(value)) {
            if (type.isPrimitive()) {
                type = PrimitiveTypes.getWrapper(type);
            }
            if (!type.isInstance(value)) {
                throw new BeanReflectiveException(value.getClass() + "[" + value + "] can not cast to " + type);
            }
        }
    }

    public static Object getEnum(Class<?> cls, int ordinal) {
        if (!cls.isEnum()) {
            throw new IllegalArgumentException();
        }
        Object array = SINGLE_ENUM_MAP.computeIfAbsent(cls, k -> {
            try {
                Method method = cls.getMethod("values");
                checkAccessible(method, null);
                return method.invoke(null);
            } catch (IllegalAccessException | InvocationTargetException | NoSuchMethodException e) {
                throw Exceptions.sneakyThrow(e);
            }
        });
        return Array.get(array, ordinal);
    }

    public static Object getEnum(Class<?> cls, String name) {
        if (!cls.isEnum()) {
            throw new IllegalArgumentException();
        }
        try {
            Method method = cls.getMethod("valueOf");
            checkAccessible(method, null);
            return method.invoke(name);
        } catch (IllegalAccessException | InvocationTargetException | NoSuchMethodException e) {
            throw Exceptions.sneakyThrow(e);
        }
    }

}
