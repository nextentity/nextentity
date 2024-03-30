package io.github.nextentity.core.reflect;

import io.github.nextentity.core.api.Lists;

import java.util.List;

/**
 * @author HuangChengwei
 * @since 2024-03-25 13:04
 */
public class PrimitiveTypes {

    private static final List<Class<?>> PRIMITIVES = Lists.of(
            Boolean.TYPE, Character.TYPE, Byte.TYPE, Short.TYPE, Integer.TYPE, Long.TYPE, Float.TYPE, Double.TYPE, Void.TYPE
    );

    private static final List<Class<?>> WRAPPERS = Lists.of(
            Boolean.class, Character.class, Byte.class, Short.class, Integer.class, Long.class, Float.class, Double.class, Void.class
    );

    public static Class<?> getWrapper(Class<?> c) {
        return WRAPPERS.get(PRIMITIVES.indexOf(c));
    }

}
