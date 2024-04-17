package io.github.nextentity.core.meta.graph;


import io.github.nextentity.core.exception.BeanReflectiveException;
import io.github.nextentity.core.reflect.ReflectUtil;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.HashSet;

/**
 * @author HuangChengwei
 * @since 2024/4/18 下午12:55
 * <p>
 */
public interface Property extends Graph {

    String name();

    Method getter();

    Method setter();

    Field field();

    Graph declaringType();

    default int deep() {
        if (!(declaringType() instanceof Property)) {
            return 1;
        }
        return ((Property) declaringType()).deep() + 1;
    }

    default boolean circularReferenced() {
        HashSet<Object> set = new HashSet<>();
        Graph cur = this;
        while (cur != null) {
            if (!set.add(cur.javaType())) {
                return true;
            }
            if (cur instanceof Property) {
                cur = ((Property) cur).declaringType();
            } else {
                cur = null;
            }
        }
        return false;
    }

    default Object get(Object entity) {
        try {
            Method getter = getter();
            if (getter != null && ReflectUtil.isAccessible(getter, entity)) {
                return getter.invoke(entity);
            } else {
                return ReflectUtil.getFieldValue(field(), entity);
            }
        } catch (IllegalAccessException | InvocationTargetException e) {
            throw new BeanReflectiveException(e);
        }
    }

    default void set(Object entity, Object value) {
        try {
            Method setter = setter();
            if (setter != null && ReflectUtil.isAccessible(setter, entity)) {
                ReflectUtil.typeCheck(value, setter.getParameterTypes()[0]);
                setter.invoke(entity, value);
            } else {
                ReflectUtil.typeCheck(value, field().getType());
                ReflectUtil.setFieldValue(field(), entity, value);
            }
        } catch (IllegalAccessException | InvocationTargetException e) {
            throw new BeanReflectiveException(e);
        }
    }

}
