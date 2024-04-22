package io.github.nextentity.core.meta;

import io.github.nextentity.core.reflect.schema.Attribute;

public interface ProjectionBasicAttribute extends Attribute {

    BasicAttribute entityAttribute();

    ProjectionType declareBy();

}
