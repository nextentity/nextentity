package io.github.nextentity.core.meta;

import lombok.SneakyThrows;

import java.lang.reflect.Method;

/**
 * @author HuangChengwei
 * @since 2024/4/24 下午3:08
 */
public class OrdinalOfEnumType implements DatabaseType {
    private final Class<?> databaseType;
    private final Object[] values;

    @SneakyThrows
    public OrdinalOfEnumType(Class<?> attributeType) {
        this.databaseType = Integer.class;
        Method method = attributeType.getDeclaredMethod("values");
        this.values = (Object[]) method.invoke(null);
    }

    @Override
    public Class<?> databaseType() {
        return databaseType;
    }

    @Override
    public Object toDatabaseType(Object value) {
        return value == null ? null : ((Enum<?>) value).ordinal();
    }

    @Override
    public Object toAttributeType(Object value) {
        return value instanceof Integer ? values[(Integer) value] : value;
    }
}
