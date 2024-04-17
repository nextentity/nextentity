package io.github.nextentity.core.meta.graph;

public interface ProjectionReferenced extends ProjectionProperty, ProjectionSchema {
    EntityReferenced entityAttribute();

    default EntityReferenced entityType() {
        return entityAttribute();
    }
}
