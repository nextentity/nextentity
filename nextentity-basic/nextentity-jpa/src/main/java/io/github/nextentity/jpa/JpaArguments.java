package io.github.nextentity.jpa;

import io.github.nextentity.core.converter.TypeConverter;
import io.github.nextentity.core.meta.Metamodel;
import io.github.nextentity.core.reflect.TypedArguments;

/**
 * @author HuangChengwei
 * @since 2024/4/17 下午5:17
 */
public class JpaArguments extends TypedArguments {
    private final Object[] objects;

    public JpaArguments(Object[] objects,
                        Class<?>[] types,
                        TypeConverter typeConverter,
                        Metamodel metamodel,
                        Class<?> entityType) {
        super(types, typeConverter, metamodel.getEntity(entityType));
        this.objects = objects;
    }

    @Override
    protected Object getValue(int index, Class<?> type) {
        return objects[index];
    }
}
