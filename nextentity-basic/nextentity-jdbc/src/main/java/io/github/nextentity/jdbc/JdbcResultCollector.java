package io.github.nextentity.jdbc;

import io.github.nextentity.core.TypeCastUtil;
import io.github.nextentity.core.converter.TypeConverter;
import io.github.nextentity.jdbc.JdbcQueryExecutor.ResultCollector;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

public class JdbcResultCollector implements ResultCollector {
    private final TypeConverter typeConverter;

    public JdbcResultCollector() {
        this(TypeConverter.ofDefault());
    }

    public JdbcResultCollector(TypeConverter typeConverter) {
        this.typeConverter = typeConverter;
    }

    @Override
    public <T> List<T> resolve(ResultSet resultSet,
                               QueryContext context) throws SQLException {
        int type = resultSet.getType();
        List<Object> result;
        if (type != ResultSet.TYPE_FORWARD_ONLY) {
            resultSet.last();
            int size = resultSet.getRow();
            result = new ArrayList<>(size);
            resultSet.beforeFirst();
        } else {
            result = new ArrayList<>();
        }
        int columnsCount = resultSet.getMetaData().getColumnCount();

        Class<?>[] types = context.getSelects().stream()
                .map(context::getExpressionType)
                .toArray(Class<?>[]::new);
        if (types.length != columnsCount) {
            throw new IllegalStateException();
        }
        while (resultSet.next()) {
            JdbcArguments arguments = new JdbcArguments(resultSet, context.getEntityType(), types, typeConverter);
            Object o = context.construct(arguments);
            result.add(o);
        }
        return TypeCastUtil.cast(result);
    }


}
