package io.github.nextentity.jdbc;

import io.github.nextentity.core.converter.TypeConverter;
import io.github.nextentity.core.meta.EntityType;
import io.github.nextentity.core.reflect.TypedArguments;
import lombok.SneakyThrows;

import java.sql.ResultSet;

public class JdbcArguments extends TypedArguments {

    private final ResultSet resultSet;

    public JdbcArguments(ResultSet resultSet, EntityType entityType, Class<?>[] types, TypeConverter typeConverter) {
        super(types, typeConverter, entityType);
        this.resultSet = resultSet;
    }


    @SneakyThrows
    @Override
    protected Object getValue(int index, Class<?> type) {
        return JdbcUtil.getValue(resultSet, ++index, type);
    }


}