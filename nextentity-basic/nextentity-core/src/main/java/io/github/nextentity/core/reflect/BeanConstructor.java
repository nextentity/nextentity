package io.github.nextentity.core.reflect;

import io.github.nextentity.core.meta.Type;

public class BeanConstructor extends ObjectConstructor {
    public BeanConstructor(Type type) {
        super(type);
    }

    @Override
    public Object newInstance(Object[] arguments) {
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
