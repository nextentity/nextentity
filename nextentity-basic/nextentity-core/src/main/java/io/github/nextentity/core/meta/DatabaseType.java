package io.github.nextentity.core.meta;

/**
 * @author HuangChengwei
 * @since 2024/4/24 下午2:22
 */
public interface DatabaseType {

    Class<?> databaseType();

    Object toDatabaseType(Object value);

    Object toAttributeType(Object value);

}
