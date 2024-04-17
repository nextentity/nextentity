package io.github.nextentity.jdbc;

import io.github.nextentity.core.converter.TypeConverter;
import io.github.nextentity.core.meta.Metamodel;
import io.github.nextentity.core.reflect.ResultExtractor;
import lombok.SneakyThrows;

import java.sql.ResultSet;

public class JdbcArguments extends ResultExtractor {

    private final ResultSet resultSet;

    public JdbcArguments(ResultSet resultSet, Metamodel metamodel, Class<?>[] types, TypeConverter typeConverter, Class<?> entityType) {
        super(types, typeConverter, metamodel, entityType);
        this.resultSet = resultSet;
    }


    @SneakyThrows
    @Override
    protected Object getValue(int index, Class<?> type) {
        return JdbcUtil.getValue(resultSet, ++index, type);
    }


}