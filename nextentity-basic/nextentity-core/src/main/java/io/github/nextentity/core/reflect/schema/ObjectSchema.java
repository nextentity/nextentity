package io.github.nextentity.core.reflect.schema;

import java.util.Collection;
import java.util.Objects;

public interface ObjectSchema extends Schema {

    Collection<? extends Attribute> attributes();

    default Attribute getAttribute(String name) {
        return attributes().stream().filter(i -> Objects.equals(i.name(), name)).findFirst().orElse(null);
    }

    default Attribute getAttribute(Iterable<String> fieldNames) {
        Schema schema = this;
        for (String fieldName : fieldNames) {
            schema = ((ObjectSchema) schema).getAttribute(fieldName);
        }
        return (Attribute) schema;
    }

    default boolean isObject() {
        return true;
    }

}
