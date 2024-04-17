package io.github.nextentity.core.meta;

import io.github.nextentity.core.meta.graph.EntitySchema;
import io.github.nextentity.core.meta.graph.ProjectionSchema;

public interface Metamodel {
    EntitySchema getEntity(Class<?> type);

    ProjectionSchema getProjection(Class<?> baseType, Class<?> projectionType);

}
