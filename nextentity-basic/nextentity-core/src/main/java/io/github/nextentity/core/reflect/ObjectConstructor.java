package io.github.nextentity.core.reflect;

import io.github.nextentity.core.meta.graph.EntityProperty;
import io.github.nextentity.core.meta.graph.Graph;
import lombok.Setter;

public abstract class ObjectConstructor implements Property, InstanceConstructor {
    @Setter
    protected Property[] properties;
    protected final Graph type;
    protected boolean root;

    public ObjectConstructor(Graph type) {
        this.type = type;
    }

    @Override
    public EntityProperty attribute() {
        return (EntityProperty) type;
    }

}
