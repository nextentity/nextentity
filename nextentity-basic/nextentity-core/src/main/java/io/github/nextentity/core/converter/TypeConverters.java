package io.github.nextentity.core.converter;

import java.util.List;
import java.util.stream.Collectors;
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
                .distinct().collect(Collectors.toList());
    }

    @Override
    public Object convert(Object value, Class<?> targetType) {
        if (targetType.isInstance(value)) {
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
