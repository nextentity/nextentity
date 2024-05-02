package io.github.nextentity.jpa;

import io.github.nextentity.core.converter.TypeConverter;
import io.github.nextentity.core.reflect.TypedArguments;
import io.github.nextentity.core.reflect.schema.Typed;

import java.util.List;

/**
 * @author HuangChengwei
 * @since 2024/4/17 下午5:17
 */
public class JpaArguments extends TypedArguments {
    private final Object[] objects;

    public JpaArguments(Object[] objects,
                        List<? extends Typed> types,
                        TypeConverter typeConverter) {
        super(types, typeConverter);
        this.objects = objects;
    }

    @Override
    protected Object getValue(int index, Class<?> type) {
        return objects[index];
    }
}
