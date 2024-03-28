package io.github.nextentity.core.meta;

public interface Metamodel {
    EntityType getEntity(Class<?> type);

    Projection getProjection(Class<?> baseType, Class<?> projectionType);

}
