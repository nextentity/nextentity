package io.github.nextentity.jdbc;

import io.github.nextentity.core.ExpressionTypeResolver;
import io.github.nextentity.core.TypeCastUtil;
import io.github.nextentity.core.api.ExpressionTree.ExpressionNode;
import io.github.nextentity.core.converter.TypeConverter;
import io.github.nextentity.core.expression.QueryStructure;
import io.github.nextentity.core.expression.Selected;
import io.github.nextentity.core.meta.Metamodel;
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
                               Metamodel metamodel,
                               QueryStructure structure) throws SQLException {
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
        Selected select = structure.select();
        int columnsCount = resultSet.getMetaData().getColumnCount();

        ExpressionTypeResolver typeResolver = new ExpressionTypeResolver(metamodel);
        List<? extends ExpressionNode> expressions = select.expressions();
        Class<?>[] types = expressions.stream()
                .map(expr -> typeResolver.getExpressionType(expr, structure.from().type()))
                .toArray(Class<?>[]::new);
        if (expressions.size() != columnsCount) {
            throw new IllegalStateException();
        }
        while (resultSet.next()) {
            JdbcResultExtractor extractor = new JdbcResultExtractor(resultSet, metamodel, types, typeConverter, structure.from().type());
            Object o = extractor.extractRow(select);
            result.add(o);
        }
        return TypeCastUtil.cast(result);
    }



}
