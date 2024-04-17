package io.github.nextentity.core.meta.graph;

import java.util.Collection;

public interface EntitySchema extends Schema {

    EntityProperty id();

    String tableName();

    EntityProperty getProperty(String fieldName);

    EntityProperty version();

    @Override
    Collection<? extends EntityProperty> properties();
}
