package io.github.nextentity.core.meta.graph;

public interface ProjectionProperty extends Property {

    EntityProperty entityAttribute();

    ProjectionSchema declaringType();

}
