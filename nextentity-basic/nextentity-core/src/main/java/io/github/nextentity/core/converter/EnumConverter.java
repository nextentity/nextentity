package io.github.nextentity.core.converter;

import io.github.nextentity.core.reflect.ReflectUtil;

/**
 * @author HuangChengwei
 * @since 2024-03-25 11:57
 */
public class EnumConverter implements TypeConverter {

    private static final EnumConverter INSTANCE = new EnumConverter();

    public static EnumConverter of() {
        return INSTANCE;
    }

    @Override
    public Object convert(Object input, Class<?> targetType) {
        if (!targetType.isEnum() || input == null || targetType.isInstance(input)) {
            return input;
        }
        if (input instanceof String) {
            try {
                return ReflectUtil.getEnum(targetType, (String) input);
            } catch (Exception ignore) {
            }
        }
        Object num = NumberConverter.of().convert(input, Integer.class);
        if (num instanceof Integer) {
            try {
                return ReflectUtil.getEnum(targetType, (Integer) num);
            } catch (Exception ignore) {
            }
        }
        return input;
    }

    protected EnumConverter() {
    }

}
