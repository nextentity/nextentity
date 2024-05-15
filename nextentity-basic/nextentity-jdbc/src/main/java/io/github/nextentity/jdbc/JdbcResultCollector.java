package io.github.nextentity.jdbc;

import io.github.nextentity.core.TypeCastUtil;
import io.github.nextentity.core.converter.TypeConverter;
import io.github.nextentity.core.reflect.schema.InstanceFactory.PrimitiveFactory;
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
        List<? extends PrimitiveFactory> primitives = context.getConstructor().primitives();
        if (primitives.size() != columnsCount) {
            throw new IllegalStateException();
        }
        while (resultSet.next()) {
            JdbcArguments arguments = new JdbcArguments(resultSet, primitives, typeConverter);
            Object o = context.construct(arguments);
            result.add(o);
        }
        return TypeCastUtil.cast(result);
    }


}
