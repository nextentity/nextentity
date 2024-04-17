package io.github.nextentity.jpa;

import io.github.nextentity.core.converter.TypeConverter;
import io.github.nextentity.core.meta.Metamodel;
import io.github.nextentity.core.reflect.ResultExtractor;

/**
 * @author HuangChengwei
 * @since 2024/4/17 下午5:17
 */
public class JpaResultExtractor extends ResultExtractor {
    private final Object[] objects;

    public JpaResultExtractor(Object[] objects,
                              Class<?>[] types,
                              TypeConverter typeConverter,
                              Metamodel metamodel,
                              Class<?> entityType) {
        super(types, typeConverter, metamodel, entityType);
        this.objects = objects;
    }

    @Override
    protected Object getValue(int index, Class<?> type) {
        return objects[index];
    }
}
