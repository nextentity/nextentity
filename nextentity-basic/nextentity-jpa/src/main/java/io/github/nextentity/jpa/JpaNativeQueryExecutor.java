package io.github.nextentity.jpa;

import io.github.nextentity.core.QueryExecutor;
import io.github.nextentity.core.TypeCastUtil;
import io.github.nextentity.core.converter.TypeConverter;
import io.github.nextentity.core.expression.QueryStructure;
import io.github.nextentity.core.meta.Metamodel;
import io.github.nextentity.core.reflect.schema.InstanceFactory.PrimitiveFactory;
import io.github.nextentity.jdbc.JdbcQueryExecutor.QuerySqlBuilder;
import io.github.nextentity.jdbc.QueryContext;
import io.github.nextentity.jdbc.QuerySqlStatement;
import javax.persistence.EntityManager;
import lombok.extern.slf4j.Slf4j;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;

@Slf4j
public class JpaNativeQueryExecutor implements QueryExecutor {
    private final QuerySqlBuilder sqlBuilder;
    private final EntityManager entityManager;
    private final Metamodel metamodel;
    private final TypeConverter typeConverter;

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
        QueryContext context = new QueryContext(queryStructure, metamodel, true);
        QuerySqlStatement preparedSql = sqlBuilder.build(context);
        // noinspection SqlSourceToSinkFlow
        javax.persistence.Query query = entityManager.createNativeQuery(preparedSql.sql());
        int position = 0;
        for (Object arg : preparedSql.parameters()) {
            query.setParameter(++position, arg);
        }
        List<?> list = TypeCastUtil.cast(query.getResultList());

        return resolve(list, context);
    }

    protected <T> List<T> resolve(
            List<?> resultSet,
            QueryContext context) {
        List<Object> result = new ArrayList<>(resultSet.size());
        if (resultSet.isEmpty()) {
            return TypeCastUtil.cast(result);
        }
        Object first = resultSet.get(0);
        int columnsCount = asArray(first).length;
        List<? extends PrimitiveFactory> expressions = context.getConstructor().primitives();
        if (expressions.size() != columnsCount) {
            throw new IllegalStateException("column count error");
        }

        for (Object o : resultSet) {
            Object[] array = asArray(o);
            JpaArguments arguments = new JpaArguments(array, expressions, typeConverter);
            Object row = context.construct(arguments);
            result.add(row);
        }
        return TypeCastUtil.cast(result);
    }

    private Object[] asArray(Object r) {
        if (r instanceof Object[]) {
            return (Object[]) r;
        }
        return new Object[]{r};
    }
}
