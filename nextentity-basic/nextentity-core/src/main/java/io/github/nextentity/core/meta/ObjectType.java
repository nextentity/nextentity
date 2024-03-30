package io.github.nextentity.core.meta;

import java.util.Collection;

public interface ObjectType extends Type {
    Collection<? extends Attribute> attributes();
}
