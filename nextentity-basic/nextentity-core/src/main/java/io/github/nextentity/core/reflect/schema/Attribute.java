package io.github.nextentity.core.reflect.schema;


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
public interface Attribute extends Schema {

    String name();

    Method getter();

    Method setter();

    Field field();

    Schema declareBy();

    default boolean isAttribute() {
        return true;
    }

    default int deep() {
        if (!(declareBy() instanceof Attribute)) {
            return 1;
        }
        return ((Attribute) declareBy()).deep() + 1;
    }

    default boolean circularReferenced() {
        HashSet<Object> set = new HashSet<>();
        Schema cur = this;
        while (cur != null) {
            if (!set.add(cur.type())) {
                return true;
            }
            cur = cur.declareBy();
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
