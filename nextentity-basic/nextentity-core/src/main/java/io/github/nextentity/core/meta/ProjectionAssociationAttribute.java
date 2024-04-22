package io.github.nextentity.core.meta;

public interface ProjectionAssociationAttribute extends ProjectionBasicAttribute, ProjectionType {
    AssociationAttribute entityAttribute();

    default AssociationAttribute entityType() {
        return entityAttribute();
    }
}
