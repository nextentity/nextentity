package io.github.nextentity.core.reflect;

import io.github.nextentity.core.meta.Attribute;
import io.github.nextentity.core.meta.Type;
import lombok.Setter;

public abstract class ObjectConstructor implements Property, InstanceConstructor {
    @Setter
    protected Property[] properties;
    protected final Type type;
    protected boolean root;

    public ObjectConstructor(Type type) {
        this.type = type;
    }

    @Override
    public Attribute attribute() {
        return (Attribute) type;
    }

}
