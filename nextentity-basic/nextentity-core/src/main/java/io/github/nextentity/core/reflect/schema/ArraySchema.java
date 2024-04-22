package io.github.nextentity.core.reflect.schema;

import java.util.Collection;

public interface ArraySchema extends Schema {
    Collection<? extends Schema> items();

    default boolean isArray() {
        return true;
    }
}
