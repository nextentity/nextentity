package io.github.nextentity.core.reflect.schema;

import java.lang.reflect.Field;
import java.lang.reflect.Method;

public interface AttributeFaced extends Attribute {

    Attribute attribute();

    @Override
    default String name() {
        return attribute().name();
    }

    @Override
    default Method getter() {
        return attribute().getter();
    }

    @Override
    default Method setter() {
        return attribute().setter();
    }

    @Override
    default Field field() {
        return attribute().field();
    }

    @Override
    default Schema declareBy() {
        return attribute().declareBy();
    }

    @Override
    default Class<?> type() {
        return attribute().type();
    }
}
