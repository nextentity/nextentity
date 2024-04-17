package io.github.nextentity.core.reflect;

import io.github.nextentity.core.meta.graph.EntityProperty;

public class PropertyImpl implements Property {
    private int index;
    private final EntityProperty attribute;

    public PropertyImpl(EntityProperty attribute) {
        this.attribute = attribute;
    }

    @Override
    public EntityProperty attribute() {
        return attribute;
    }

    @Override
    public Object newInstance(Arguments arguments) {
        return arguments.get(index);
    }

    void setIndex(int index) {
        this.index = index;
    }
}
