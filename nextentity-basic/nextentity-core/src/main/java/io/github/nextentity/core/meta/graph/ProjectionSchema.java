package io.github.nextentity.core.meta.graph;

import java.util.Collection;

public interface ProjectionSchema extends Schema {

    Collection<? extends ProjectionProperty> properties();

    EntitySchema entityType();

}
