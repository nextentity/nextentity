package io.github.nextentity.jpa;

import io.github.nextentity.core.QueryExecutor;
import io.github.nextentity.core.ExpressionTypeResolver;
import io.github.nextentity.core.Tuples;
import io.github.nextentity.core.TypeCastUtil;
import io.github.nextentity.core.api.Expression;
import io.github.nextentity.core.api.Expression.QueryStructure;
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
import io.github.nextentity.jdbc.ConnectionProvider;
import io.github.nextentity.jdbc.JdbcQueryExecutor;
import io.github.nextentity.jdbc.JdbcQueryExecutor.PreparedSql;
import io.github.nextentity.jdbc.JdbcQueryExecutor.QuerySqlBuilder;
import io.github.nextentity.jdbc.JdbcResultCollector;
import jakarta.persistence.EntityManager;
import lombok.extern.slf4j.Slf4j;
import org.hibernate.engine.jdbc.connections.spi.JdbcConnectionAccess;
import org.hibernate.engine.spi.SharedSessionContractImplementor;
import org.jetbrains.annotations.NotNull;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

@Slf4j
public class JpaNativeQueryExecutor implements QueryExecutor {
    private final QuerySqlBuilder sqlBuilder;
    private final EntityManager entityManager;
    private final Metamodel metamodel;
    private final TypeConverter typeConverter;

    public static QueryExecutor getExecutor(QuerySqlBuilder sqlBuilder,
                                            EntityManager entityManager,
                                            Metamodel metamodel,
                                            TypeConverter typeConverter) {
        try {
            ConnectionProvider connectionProvider = getConnectionProvider(entityManager);
            if (connectionProvider != null) {
                return new JdbcQueryExecutor(metamodel, sqlBuilder, connectionProvider, new JdbcResultCollector(typeConverter));
            }
        } catch (Throwable e) {
            log.info("can not create JdbcQueryExecutor", e);
        }
        return new JpaNativeQueryExecutor(sqlBuilder, entityManager, metamodel, typeConverter);
    }

    private static ConnectionProvider getConnectionProvider(EntityManager entityManager) {
        Object delegate = entityManager.getDelegate();
        if (delegate instanceof SharedSessionContractImplementor) {
            return new ConnectionProvider() {
                @Override
                public <T> T execute(ConnectionCallback<T> action) throws SQLException {
                    JdbcConnectionAccess access = ((SharedSessionContractImplementor) entityManager.getDelegate())
                            .getJdbcConnectionAccess();
                    Connection connection = access.obtainConnection();
                    try {
                        return action.doInConnection(connection);
                    } finally {
                        access.releaseConnection(connection);
                    }
                }
            };
        }
        return null;
    }

    public JpaNativeQueryExecutor(QuerySqlBuilder sqlBuilder,
                                  EntityManager entityManager,
                                  Metamodel metamodel,
                                  TypeConverter typeConverter) {
        this.sqlBuilder = sqlBuilder;
        this.entityManager = entityManager;
        this.metamodel = metamodel;
        this.typeConverter = typeConverter;
    }

    @Override
    public <T> List<T> getList(@NotNull QueryStructure queryStructure) {
        return queryByNativeSql(queryStructure);
    }

    private <T> List<T> queryByNativeSql(@NotNull QueryStructure queryStructure) {
        PreparedSql preparedSql = sqlBuilder.build(queryStructure, metamodel);
        jakarta.persistence.Query query = entityManager.createNativeQuery(preparedSql.sql());
        int position = 0;
        for (Object arg : preparedSql.args()) {
            query.setParameter(++position, arg);
        }
        List<?> list = TypeCastUtil.cast(query.getResultList());

        return resolve(list, preparedSql.selected(), queryStructure);
    }

    protected <T> List<T> resolve(
            List<?> resultSet,
            List<? extends Attribute> selected,
            QueryStructure structure) {
        List<T> result = new ArrayList<>(resultSet.size());
        if (resultSet.isEmpty()) {
            return result;
        }
        Selection select = structure.select();
        int columnsCount = asArray(resultSet.get(0)).length;

        if (select instanceof MultiSelected multiSelected) {
            if (multiSelected.expressions().size() != columnsCount) {
                throw new IllegalStateException("column count error");
            }
            ExpressionTypeResolver typeResolver = new ExpressionTypeResolver(metamodel);
            List<Class<?>> types = multiSelected.expressions().stream()
                    .map(expr -> typeResolver.getExpressionType(expr, structure.from().type()))
                    .collect(Collectors.toList());
            for (Object r : resultSet) {
                Object[] row = asArray(r);
                if (typeConverter != null) {
                    for (int i = 0; i < types.size(); i++) {
                        Class<?> attribute = types.get(i);
                        row[i] = typeConverter.convert(row[i], attribute);
                    }
                }
                result.add(TypeCastUtil.unsafeCast(Tuples.of(row)));
            }
        } else if (select instanceof SingleSelected) {
            if (1 != columnsCount) {
                throw new IllegalStateException();
            }
            Expression expression = ((SingleSelected) select).expression();
            ExpressionTypeResolver typeResolver = new ExpressionTypeResolver(metamodel);
            Class<?> type = typeResolver.getExpressionType(expression, structure.from().type());
            for (Object r : resultSet) {
                Object val = asArray(r)[0];
                val = typeConverter.convert(val, type);
                result.add(TypeCastUtil.unsafeCast(val));
            }
        } else {
            if (selected.size() != columnsCount) {
                throw new IllegalStateException();
            }
            Class<?> resultType = getResultType(structure);
            InstanceConstructor extractor = ReflectUtil.getRowInstanceConstructor(selected, resultType);
            for (Object r : resultSet) {
                Object[] array = asArray(r);
                if (typeConverter != null) {
                    for (int i = 0; i < selected.size(); i++) {
                        Attribute attribute = selected.get(i);
                        array[i] = typeConverter.convert(array[i], attribute.javaType());
                    }
                }
                T row = TypeCastUtil.unsafeCast(extractor.newInstance(array));
                result.add(row);
            }
        }
        return result;
    }

    private Object[] asArray(Object r) {
        if (r instanceof Object[]) {
            return (Object[]) r;
        }
        return new Object[]{r};
    }

    private static Class<?> getResultType(QueryStructure structure) {
        Selection select = structure.select();
        Class<?> resultType;
        if (select instanceof EntitySelected) {
            resultType = structure.from().type();
        } else if (select instanceof ProjectionSelected) {
            resultType = select.resultType();
        } else {
            throw new IllegalStateException();
        }
        return resultType;
    }
}
