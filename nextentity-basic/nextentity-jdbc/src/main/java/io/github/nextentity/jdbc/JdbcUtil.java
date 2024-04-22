package io.github.nextentity.jdbc;

import io.github.nextentity.core.TypeCastUtil;
import io.github.nextentity.core.reflect.ReflectUtil;

import java.math.BigDecimal;
import java.sql.Blob;
import java.sql.Clob;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Time;
import java.sql.Timestamp;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

public abstract class JdbcUtil {

    private static final Map<Class<?>, ResultSetGetter<?>> GETTER_MAPS = new HashMap<>();

    static {

        put(Byte.class, ResultSet::getByte);
        put(byte.class, ResultSet::getByte);
        put(Short.class, ResultSet::getShort);
        put(short.class, ResultSet::getShort);
        put(Integer.class, ResultSet::getInt);
        put(int.class, ResultSet::getInt);
        put(Float.class, ResultSet::getFloat);
        put(float.class, ResultSet::getFloat);
        put(Long.class, ResultSet::getLong);
        put(long.class, ResultSet::getLong);
        put(Double.class, ResultSet::getDouble);
        ResultSetGetter<Character> getChar = (resultSet, index) -> {
            String string = resultSet.getString(index);
            if (string == null || string.length() != 1) {
                throw new IllegalStateException(string + " is not a character");
            }
            return string.charAt(0);
        };
        put(char.class, getChar);
        put(Character.class, getChar);
        put(double.class, ResultSet::getDouble);
        put(Boolean.class, ResultSet::getBoolean);
        put(boolean.class, ResultSet::getBoolean);
        put(BigDecimal.class, ResultSet::getBigDecimal);
        put(Date.class, ResultSet::getTimestamp);
        put(String.class, ResultSet::getString);
        put(Time.class, ResultSet::getTime);

        put(java.sql.Date.class, ResultSet::getDate);
        put(Blob.class, ResultSet::getBlob);
        put(Clob.class, ResultSet::getClob);
        put(java.sql.Array.class, ResultSet::getArray);
        put(java.io.InputStream.class, ResultSet::getBinaryStream);
        put(byte[].class, ResultSet::getBytes);
        put(Timestamp.class, ResultSet::getTimestamp);
        put(Instant.class, (resultSet, columnIndex) -> {
            Timestamp timestamp = resultSet.getTimestamp(columnIndex);
            return timestamp.toInstant();
        });
        put(LocalDate.class, (resultSet, columnIndex) -> resultSet.getDate(columnIndex).toLocalDate());
        put(LocalDateTime.class, (resultSet, columnIndex) -> resultSet.getTimestamp(columnIndex).toLocalDateTime());
        put(LocalTime.class, (resultSet, columnIndex) -> resultSet.getTime(columnIndex).toLocalTime());

    }

    public static Object getValue(ResultSet resultSet, int column, Class<?> targetType) throws SQLException {
        Object result = resultSet.getObject(column);
        if (result == null) {
            return null;
        }
        if (!targetType.isInstance(result)) {
            ResultSetGetter<?> getter = GETTER_MAPS.get(targetType);
            if (getter == null) {
                if (Enum.class.isAssignableFrom(targetType)) {
                    result = ReflectUtil.getEnum(targetType, resultSet.getInt(column));
                }
            } else {
                result = getter.getValue(resultSet, column);
            }
        }
        return TypeCastUtil.unsafeCast(result);
    }

    public static void setParam(PreparedStatement pst, Iterable<?> args) throws SQLException {
        int i = 0;
        for (Object arg : args) {
            if (arg instanceof Enum) {
                arg = ((Enum<?>) arg).ordinal();
            }
            pst.setObject(++i, arg);
        }
    }

    private static <T> void put(Class<T> type, ResultSetGetter<T> getter) {
        GETTER_MAPS.put(type, getter);
    }

    @FunctionalInterface
    interface ResultSetGetter<T> {
        T getValue(ResultSet resultSet, int index) throws SQLException;
    }
}

