package io.github.nextentity.core.reflect;

import io.github.nextentity.core.meta.Attribute;

public class PropertyImpl implements Property {
    private int index;
    private final Attribute attribute;

    public PropertyImpl(Attribute attribute) {
        this.attribute = attribute;
    }

    @Override
    public Attribute attribute() {
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
