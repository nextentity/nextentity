package io.github.nextentity.core.reflect;

import io.github.nextentity.core.util.Maps;
import org.jetbrains.annotations.NotNull;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

/**
 * @author HuangChengwei
 * @since 2024-03-25 13:04
 */
public class PrimitiveTypes {

    private static final Map<Class<?>, Class<?>> PRIMITIVE_WRAPPER =
            Maps.<Class<?>, Class<?>>hashmap().put(primitives(), wrappers()).build();
    private static final Map<Class<?>, Class<?>> WRAPPER_PRIMITIVE =
            Maps.<Class<?>, Class<?>>hashmap().put(wrappers(), primitives()).build();

    public static Class<?> getWrapper(Class<?> c) {
        return PRIMITIVE_WRAPPER.getOrDefault(c, c);
    }

    public static boolean isBasicType(Class<?> c) {
        return c.isPrimitive() || WRAPPER_PRIMITIVE.getOrDefault(c, c).isPrimitive();
    }

    @NotNull
    private static List<Class<?>> wrappers() {
        return Arrays.asList(Boolean.class, Character.class, Byte.class, Short.class,
                Integer.class, Long.class, Float.class, Double.class, Void.class);
    }

    @NotNull
    private static List<Class<?>> primitives() {
        return Arrays.asList(Boolean.TYPE, Character.TYPE, Byte.TYPE, Short.TYPE,
                Integer.TYPE, Long.TYPE, Float.TYPE, Double.TYPE, Void.TYPE);
    }

}
