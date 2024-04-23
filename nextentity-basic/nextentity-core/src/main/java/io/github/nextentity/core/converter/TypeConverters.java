package io.github.nextentity.core.converter;

import io.github.nextentity.core.reflect.PrimitiveTypes;
import io.github.nextentity.core.util.ImmutableList;

import java.util.List;
import java.util.stream.Stream;

/**
 * @author HuangChengwei
 * @since 2024-03-25 12:43
 */
public class TypeConverters implements TypeConverter {
    private final List<TypeConverter> converters;

    public TypeConverters(List<TypeConverter> converters) {
        this.converters = converters.stream()
                .flatMap(converter -> {
                    if (converter.getClass() == TypeConverters.class) {
                        return ((TypeConverters) converter).converters.stream();
                    } else {
                        return Stream.of(converter);
                    }
                })
                .distinct().collect(ImmutableList.collector(converters.size()));
    }

    @Override
    public Object convert(Object value, Class<?> targetType) {
        if (targetType.isInstance(value)) {
            return value;
        }
        if (targetType.isPrimitive() && PrimitiveTypes.getWrapper(targetType).isInstance(value)) {
            return value;
        }
        for (TypeConverter converter : converters) {
            value = converter.convert(value, targetType);
            if (targetType.isInstance(value)) {
                return value;
            }
        }
        return value;
    }
}
