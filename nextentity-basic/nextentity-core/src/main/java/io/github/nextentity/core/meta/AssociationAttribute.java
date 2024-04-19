package io.github.nextentity.core.meta;

import io.github.nextentity.core.reflect.schema.Attribute;
import io.github.nextentity.core.reflect.schema.ObjectSchema;

public interface AssociationAttribute extends BasicAttribute, EntitySchema, Attribute, ObjectSchema {

    String joinColumnName();

    String joinName();

    String referencedColumnName();
}
