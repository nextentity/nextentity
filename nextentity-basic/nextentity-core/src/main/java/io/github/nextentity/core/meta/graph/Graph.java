package io.github.nextentity.core.meta.graph;

public interface Graph {

    default boolean isBasic() {
        return !isSchema();
    }

    default boolean isSchema() {
        return this instanceof Schema;
    }


    Class<?> javaType();

}
