package io.github.nextentity.jpa;

import io.github.nextentity.core.ExpressionTypeResolver;
import io.github.nextentity.core.QueryExecutor;
import io.github.nextentity.core.SqlStatement;
import io.github.nextentity.core.TypeCastUtil;
import io.github.nextentity.core.api.ExpressionTree.ExpressionNode;
import io.github.nextentity.core.converter.TypeConverter;
import io.github.nextentity.core.expression.QueryStructure;
import io.github.nextentity.core.expression.Selected;
import io.github.nextentity.core.meta.Metamodel;
import io.github.nextentity.jdbc.JdbcQueryExecutor.QuerySqlBuilder;
import jakarta.persistence.EntityManager;
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

    @Override
    public Metamodel metamodel() {
        return metamodel;
    }

    private <T> List<T> queryByNativeSql(@NotNull QueryStructure queryStructure) {
        SqlStatement<?> preparedSql = sqlBuilder.build(queryStructure, metamodel);
        jakarta.persistence.Query query = entityManager.createNativeQuery(preparedSql.getSql());
        int position = 0;
        for (Object arg : preparedSql.getParameters()) {
            query.setParameter(++position, arg);
        }
        List<?> list = TypeCastUtil.cast(query.getResultList());

        return resolve(list, queryStructure);
    }

    protected <T> List<T> resolve(
            List<?> resultSet,
            QueryStructure structure) {
        List<Object> result = new ArrayList<>(resultSet.size());
        if (resultSet.isEmpty()) {
            return TypeCastUtil.cast(result);
        }
        Selected select = structure.select();
        Object first = resultSet.get(0);
        int columnsCount = asArray(first).length;
        if (select.expressions().size() != columnsCount) {
            throw new IllegalStateException("column count error");
        }

        ExpressionTypeResolver typeResolver = new ExpressionTypeResolver(metamodel);
        List<? extends ExpressionNode> expressions = select.expressions();
        Class<?>[] types = expressions.stream()
                .map(expr -> typeResolver.getExpressionType(expr, structure.from().type()))
                .toArray(Class<?>[]::new);
        if (expressions.size() != columnsCount) {
            throw new IllegalStateException();
        }
        for (Object o : resultSet) {
            Object[] array = asArray(o);
            JpaResultExtractor extractor = new JpaResultExtractor(array, types, typeConverter, metamodel, structure.from().type());
            Object row = extractor.extractRow(select);
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
