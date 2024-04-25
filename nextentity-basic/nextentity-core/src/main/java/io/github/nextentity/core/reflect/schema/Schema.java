package io.github.nextentity.core.reflect.schema;

public interface Schema {

    Class<?> type();

    default String name() {
        throw new UnsupportedOperationException();
    }

    default Schema declareBy() {
        throw new UnsupportedOperationException();
    }

    default boolean isObject() {
        return false;
    }

    default boolean isArray() {
        return false;
    }

    default boolean isPrimitive() {
        return !isObject() && !isArray();
    }

    default boolean isAttribute() {
        return false;
    }

}
