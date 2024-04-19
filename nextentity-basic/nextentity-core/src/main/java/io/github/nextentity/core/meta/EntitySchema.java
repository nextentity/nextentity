package io.github.nextentity.core.meta;

import io.github.nextentity.core.reflect.schema.ObjectSchema;
import io.github.nextentity.core.reflect.schema.Schema;

import java.util.Collection;

public interface EntitySchema extends ObjectSchema {

    BasicAttribute id();

    String tableName();

    BasicAttribute getAttribute(String fieldName);

    default BasicAttribute getAttribute(Iterable<String> fieldNames) {
        Schema attr = ObjectSchema.super.getAttribute(fieldNames);
        return (BasicAttribute) attr;
    }

    BasicAttribute version();

    @Override
    Collection<? extends BasicAttribute> attributes();
}
