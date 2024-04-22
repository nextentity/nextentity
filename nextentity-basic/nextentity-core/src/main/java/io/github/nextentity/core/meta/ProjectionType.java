package io.github.nextentity.core.meta;

import io.github.nextentity.core.reflect.Arguments;
import io.github.nextentity.core.reflect.ObjectFactory;
import io.github.nextentity.core.reflect.SelectedConstruct;
import io.github.nextentity.core.reflect.schema.ObjectSchema;

import java.util.Collection;

public interface ProjectionType extends ObjectSchema, ObjectFactory {

    Collection<? extends ProjectionBasicAttribute> attributes();

    EntitySchema entityType();

    SelectedConstruct constructor();

    @Override
    default Object get(Arguments arguments) {
        return constructor().get(arguments);
    }
}
