package io.github.nextentity.core.meta;

import io.github.nextentity.core.api.expression.EntityPath;
import io.github.nextentity.core.reflect.schema.Attribute;
import io.github.nextentity.core.reflect.schema.Schema;
import io.github.nextentity.core.util.ImmutableList;

import java.util.ArrayDeque;
import java.util.List;

public interface BasicAttribute extends Schema, Attribute {

    String columnName();

    boolean isVersion();

    EntitySchema declareBy();

    @Override
    default int deep() {
        return attributePaths().size();
    }

    EntityPath path();

    default List<? extends BasicAttribute> attributePaths() {
        Schema cur = this;
        ArrayDeque<BasicAttribute> attributes = new ArrayDeque<>(2);
        while (true) {
            if (cur instanceof BasicAttribute) {
                // noinspection PatternVariableCanBeUsed
                BasicAttribute attribute = (BasicAttribute) cur;
                attributes.addFirst(attribute);
                cur = attribute.declareBy();
            } else {
                break;
            }
        }
        return new ImmutableList<>(attributes);
    }
}
