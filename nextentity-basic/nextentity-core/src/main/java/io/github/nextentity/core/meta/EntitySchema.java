package io.github.nextentity.core.meta;

import io.github.nextentity.core.reflect.schema.ObjectSchema;
import io.github.nextentity.core.reflect.schema.Schema;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

public interface EntitySchema extends ObjectSchema {

    BasicAttribute id();

    String tableName();

    BasicAttribute getAttribute(String fieldName);

    @Override
    default List<? extends BasicAttribute> primitiveAttributes() {
        return attributes().stream().filter(Schema::isPrimitive).collect(Collectors.toList());
    }

    default BasicAttribute getAttribute(Iterable<String> fieldNames) {
        Schema attr = ObjectSchema.super.getAttribute(fieldNames);
        return (BasicAttribute) attr;
    }

    BasicAttribute version();

    @Override
    Collection<? extends BasicAttribute> attributes();
}
