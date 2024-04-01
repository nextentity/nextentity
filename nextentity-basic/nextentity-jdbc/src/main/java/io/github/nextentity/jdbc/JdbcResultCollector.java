package io.github.nextentity.jdbc;

import io.github.nextentity.core.ExpressionTypeResolver;
import io.github.nextentity.core.Tuples;
import io.github.nextentity.core.TypeCastUtil;
import io.github.nextentity.core.api.QueryStructure;
import io.github.nextentity.core.api.Selection;
import io.github.nextentity.core.api.Selection.EntitySelected;
import io.github.nextentity.core.api.Selection.MultiSelected;
import io.github.nextentity.core.api.Selection.ProjectionSelected;
import io.github.nextentity.core.api.Selection.SingleSelected;
import io.github.nextentity.core.converter.TypeConverter;
import io.github.nextentity.core.meta.Attribute;
import io.github.nextentity.core.meta.Metamodel;
import io.github.nextentity.core.reflect.InstanceConstructor;
import io.github.nextentity.core.reflect.ReflectUtil;
import io.github.nextentity.jdbc.JdbcQueryExecutor.ResultCollector;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

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
                               List<? extends Attribute> selected,
                               QueryStructure structure) throws SQLException {
        int type = resultSet.getType();
        List<T> result;
        if (type != ResultSet.TYPE_FORWARD_ONLY) {
            resultSet.last();
            int size = resultSet.getRow();
            result = new ArrayList<>(size);
            resultSet.beforeFirst();
        } else {
            result = new ArrayList<>();
        }
        Selection select = structure.select();
        int columnsCount = resultSet.getMetaData().getColumnCount();

        if (select instanceof MultiSelected multiSelected) {
            if (multiSelected.expressions().size() != columnsCount) {
                throw new IllegalStateException();
            }
            ExpressionTypeResolver typeResolver = new ExpressionTypeResolver(metamodel);
            List<Class<?>> types = multiSelected.expressions().stream()
                    .map(expr -> typeResolver.getExpressionType(expr, structure.from().type()))
                    .collect(Collectors.toList());
            while (resultSet.next()) {
                Object[] row = getObjects(resultSet, columnsCount, types);
                result.add(TypeCastUtil.unsafeCast(Tuples.of(row)));
            }
        } else if (select instanceof SingleSelected) {
            if (1 != columnsCount) {
                throw new IllegalStateException();
            }
            //noinspection PatternVariableCanBeUsed
            SingleSelected sc = (SingleSelected) select;
            while (resultSet.next()) {
                T row = getSingleObj(resultSet, sc);
                result.add(row);
            }
        } else {
            if (selected.size() != columnsCount) {
                throw new IllegalStateException();
            }
            Class<?> resultType;
            if (select instanceof EntitySelected) {
                resultType = structure.from().type();
            } else if (select instanceof ProjectionSelected) {
                resultType = select.resultType();
            } else {
                throw new IllegalStateException();
            }
            InstanceConstructor extractor = ReflectUtil.getRowInstanceConstructor(selected, resultType);
            Object[] data = new Object[columnsCount];
            while (resultSet.next()) {
                int i = 0;
                for (Attribute attribute : selected) {
                    data[i++] = getValue(resultSet, i, attribute.javaType());
                }
                T row = TypeCastUtil.unsafeCast(extractor.newInstance(data));
                result.add(row);
            }
        }
        return result;
    }

    @Nullable
    private <R> R getSingleObj(@NotNull ResultSet resultSet, SingleSelected selectClause) throws SQLException {
        Object r = getValue(resultSet, 1, selectClause.resultType());
        return TypeCastUtil.unsafeCast(r);
    }

    private Object[] getObjects(@NotNull ResultSet resultSet, int columnsCount, List<Class<?>> types) throws SQLException {
        int column = 0;
        Object[] row = new Object[columnsCount];
        for (Class<?> expression : types) {
            row[column++] = getValue(resultSet, column, expression);
        }
        return row;
    }

    protected Object getValue(ResultSet resultSet, int column, Class<?> targetType) throws SQLException {
        Object value = JdbcUtil.getValue(resultSet, column, targetType);
        if (typeConverter != null) {
            value = typeConverter.convert(value, targetType);
        }
        return value;
    }

}
