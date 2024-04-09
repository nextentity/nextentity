package io.github.nextentity.core.converter;

import io.github.nextentity.core.TypeCastUtil;
import io.github.nextentity.core.util.Maps;
import lombok.extern.slf4j.Slf4j;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Map;
import java.util.function.Function;

/**
 * @author HuangChengwei
 * @since 2024-03-25 11:57
 */
@Slf4j
public class NumberConverter implements TypeConverter {

    private static final Map<Class<? extends Number>, Function<Number, Number>> CONVERTERS =
            Maps.<Class<? extends Number>, Function<Number, Number>>hashmap()
                    .put(int.class, Number::intValue)
                    .put(Integer.class, Number::intValue)
                    .put(long.class, Number::longValue)
                    .put(Long.class, Number::longValue)
                    .put(float.class, Number::floatValue)
                    .put(Float.class, Number::floatValue)
                    .put(double.class, Number::doubleValue)
                    .put(Double.class, Number::doubleValue)
                    .put(short.class, Number::shortValue)
                    .put(Short.class, Number::shortValue)
                    .put(byte.class, Number::byteValue)
                    .put(Byte.class, Number::byteValue)
                    .put(BigInteger.class, n -> new BigDecimal(String.valueOf(n)).toBigInteger())
                    .put(BigDecimal.class, n -> new BigDecimal(String.valueOf(n)))
                    .build();

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
        Function<Number, Number> indexOfValueType = CONVERTERS.get(valueType);
        if (indexOfValueType == null) {
            return value;
        }
        Function<Number, Number> indexOfTargetType = CONVERTERS.get(targetType);
        if (indexOfTargetType == null) {
            return value;
        }

        Number number = (Number) value;
        Number result = indexOfTargetType.apply(number);
        if (isBasic(targetType) && isBasic(valueType)) {
            if (result.longValue() == number.longValue() && result.doubleValue() == number.doubleValue()) {
                return result;
            }
        } else {
            Number n = indexOfValueType.apply(result);
            if (equals(value, n)) {
                return result;
            }
        }
        return value;
    }

    private static boolean isBasic(Class<?> targetType) {
        return targetType != BigDecimal.class && targetType != BigInteger.class;
    }

    private static boolean equals(Object a, Number b) {
        return a.getClass() == b.getClass() &&
               (TypeCastUtil.<Comparable<Object>>unsafeCast(a)).compareTo(b) == 0;
    }


}
