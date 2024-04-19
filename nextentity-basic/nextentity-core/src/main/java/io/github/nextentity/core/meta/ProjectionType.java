package io.github.nextentity.core.meta;

import io.github.nextentity.core.reflect.schema.ObjectSchema;

import java.util.Collection;

public interface ProjectionType extends ObjectSchema {

    Collection<? extends ProjectionBasicAttribute> attributes();

    EntitySchema entityType();

}
