package io.github.nextentity.core.converter;

import io.github.nextentity.core.api.Lists;
import lombok.extern.slf4j.Slf4j;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.List;
import java.util.Objects;
import java.util.function.Function;

/**
 * @author HuangChengwei
 * @since 2024-03-25 11:57
 */
@Slf4j
public class NumberConverter implements TypeConverter {

    private static final List<Class<? extends Number>> BASIC_NUMBER_TYPES = Lists.of(
            int.class, long.class, float.class, double.class, byte.class, short.class,
            Integer.class, Long.class, Float.class, Double.class, Byte.class, Short.class,
            BigInteger.class, BigDecimal.class
    );
    private static final List<Function<Number, Number>> VALUE_FUNCTIONS = Lists.of(
            Number::intValue, Number::longValue, Number::floatValue,
            Number::doubleValue, Number::byteValue, Number::shortValue,
            Number::intValue, Number::longValue, Number::floatValue,
            Number::doubleValue, Number::byteValue, Number::shortValue,
            n -> new BigDecimal(String.valueOf(n)).toBigInteger(),
            n -> new BigDecimal(String.valueOf(n))
    );

    private static final NumberConverter INSTANCE = new NumberConverter();

    public static NumberConverter of() {
        return INSTANCE;
    }

    protected NumberConverter() {
    }

    @Override
    public Object convert(Object value, Class<?> targetType) {
        if (value == null || targetType.isInstance(value)) {
            return value;
        }
        try {
            return doConvert(value, targetType);
        } catch (Exception e) {
            log.warn(value.getClass() + "[" + value + "] cast to " + targetType + " failed", e);
            return value;
        }
    }

    private static Object doConvert(Object value, Class<?> targetType) {
        if (value instanceof String) {
            try {
                value = Double.parseDouble((String) value);
            } catch (NumberFormatException e) {
                return value;
            }
        }
        Class<?> valueType = value.getClass();
        int indexOfValueType = BASIC_NUMBER_TYPES.indexOf(valueType);
        if (indexOfValueType < 0) {
            return value;
        }
        int indexOfTargetType = BASIC_NUMBER_TYPES.indexOf(targetType);
        if (indexOfTargetType < 0) {
            return value;
        }

        Number result = VALUE_FUNCTIONS.get(indexOfTargetType).apply((Number) value);
        Number n = VALUE_FUNCTIONS.get(indexOfValueType).apply(result);
        if (Objects.equals(value, n)) {
            return result;
        }
        return value;
    }


}
