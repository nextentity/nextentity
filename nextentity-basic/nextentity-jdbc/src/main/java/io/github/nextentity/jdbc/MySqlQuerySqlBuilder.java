package io.github.nextentity.jdbc;

import io.github.nextentity.core.api.ExpressionTree.ExpressionNode;
import io.github.nextentity.core.api.ExpressionTree.QueryStructure;
import io.github.nextentity.core.meta.Attribute;
import io.github.nextentity.core.meta.Metamodel;
import io.github.nextentity.jdbc.JdbcQueryExecutor.PreparedSql;
import io.github.nextentity.jdbc.JdbcQueryExecutor.QuerySqlBuilder;
import lombok.experimental.Accessors;

import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

public class MySqlQuerySqlBuilder implements QuerySqlBuilder {


    @Override
    public PreparedSql build(QueryStructure structure, Metamodel metamodel) {
        return new Builder(structure, metamodel).build();
    }

    static class Builder extends AbstractQuerySqlBuilder {


        public Builder(StringBuilder sql,
                       List<Object> args,
                       QueryStructure queryStructure,
                       Metamodel mappers,
                       AtomicInteger selectIndex,
                       int subIndex) {
            super(sql, args, queryStructure, mappers, selectIndex, subIndex);
        }

        public Builder(QueryStructure queryStructure, Metamodel mappers) {
            super(queryStructure, mappers);
        }

        @Override
        protected String leftQuotedIdentifier() {
            return "`";
        }

        @Override
        protected String rightQuotedIdentifier() {
            return "`";
        }

        @Override
        protected void appendQueryStructure(QueryStructure queryStructure) {
            new Builder(sql, args, queryStructure, mappers, selectIndex, subIndex + 1).doBuilder();
        }

        @Override
        protected void appendPredicate(ExpressionNode node) {
            appendExpression(node);
        }

        @Override
        protected void appendOffsetAndLimit() {
            int offset = unwrap(queryStructure.offset());
            int limit = unwrap(queryStructure.limit());
            if (offset > 0) {
                sql.append(" limit ?,?");
                args.add(offset);
                args.add(limit < 0 ? Long.MAX_VALUE : limit);
            } else if (limit >= 0) {
                sql.append(" limit ");
                if (limit <= 1) {
                    sql.append(limit);
                } else {
                    sql.append("?");
                    args.add(limit);
                }
            }
        }

        private static int unwrap(Integer integer) {
            return integer == null ? -1 : integer;
        }
    }

    @lombok.Data
    @Accessors(fluent = true)
    public static final class PreparedSqlImpl implements PreparedSql {
        private final String sql;
        private final List<?> args;
        private final List<Attribute> selected;
    }
}
