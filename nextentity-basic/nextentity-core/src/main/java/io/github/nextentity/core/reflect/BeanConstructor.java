package io.github.nextentity.core.reflect;

import io.github.nextentity.core.meta.graph.Graph;

public class BeanConstructor extends ObjectConstructor {
    public BeanConstructor(Graph type) {
        super(type);
    }

    @Override
    public Object newInstance(Arguments arguments) {
        Object result = null;
        for (Property property : properties) {
            Object value = property.newInstance(arguments);
            if (value != null) {
                if (result == null) {
                    result = ReflectUtil.newInstance(type.javaType());
                }
                property.attribute().set(result, value);
            }
        }
        if (root && result == null) {
            result = ReflectUtil.newInstance(type.javaType());
        }
        return result;
    }
}
