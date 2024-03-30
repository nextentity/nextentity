package io.github.nextentity.core.converter;

import java.sql.Time;
import java.sql.Timestamp;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;

/**
 * @author HuangChengwei
 * @since 2024-03-25 13:39
 */
public class LocalDateTimeConverter implements TypeConverter {

    private static final LocalDateTimeConverter INSTANCE = new LocalDateTimeConverter();

    protected LocalDateTimeConverter() {
    }

    public static LocalDateTimeConverter of() {
        return INSTANCE;
    }

    @Override
    public Object convert(Object value, Class<?> targetType) {
        if (value == null || targetType.isInstance(value)) {
            return value;
        }
        if (value instanceof java.sql.Date && targetType == LocalDate.class) {
            return ((java.sql.Date) value).toLocalDate();
        }
        if (value instanceof Timestamp && targetType == LocalDateTime.class) {
            return ((Timestamp) value).toLocalDateTime();
        }
        if (value instanceof Time && targetType == LocalTime.class) {
            return ((Time) value).toLocalTime();
        }
        if (value instanceof java.util.Date) {
            long time = ((java.util.Date) value).getTime();
            if (targetType == LocalDate.class) {
                return new java.sql.Date(time).toLocalDate();
            }
            if (targetType == LocalDateTime.class) {
                return (new Timestamp(time)).toLocalDateTime();
            }
            if (targetType == LocalTime.class) {
                return (new Time(time)).toLocalTime();
            }
        }
        return value;
    }
}
