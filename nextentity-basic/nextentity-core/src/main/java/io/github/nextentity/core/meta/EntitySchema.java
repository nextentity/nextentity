package io.github.nextentity.core.meta;

import io.github.nextentity.core.reflect.schema.InstanceFactory;
import io.github.nextentity.core.reflect.schema.ObjectSchema;
import io.github.nextentity.core.reflect.schema.Schema;
import io.github.nextentity.core.util.ImmutableList;

import java.util.Collection;
import java.util.List;

public interface EntitySchema extends ObjectSchema {

    BasicAttribute id();

    String tableName();

    BasicAttribute getAttribute(String fieldName);

    @Override
    default List<? extends BasicAttribute> primitiveAttributes() {
        Collection<? extends BasicAttribute> attributes = attributes();
        return attributes.stream()
                .filter(Schema::isPrimitive)
                .collect(ImmutableList.collector(attributes.size()));
    }

    default BasicAttribute getAttribute(Iterable<String> fieldNames) {
        Schema attr = ObjectSchema.super.getAttribute(fieldNames);
        return (BasicAttribute) attr;
    }

    BasicAttribute version();

    InstanceFactory.ObjectFactory getInstanceFactory();

    @Override
    Collection<? extends BasicAttribute> attributes();
}
