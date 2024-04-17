package io.github.nextentity.core.meta.graph;

public interface EntityReferenced extends EntityProperty, EntitySchema {

    String joinColumnName();

    String joinName();

    String referencedColumnName();

}
