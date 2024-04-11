package io.github.nextentity.jdbc;

import io.github.nextentity.core.api.ExpressionTree.QueryStructure;
import io.github.nextentity.core.api.Operator;
import io.github.nextentity.core.meta.Metamodel;
import io.github.nextentity.jdbc.JdbcQueryExecutor.PreparedSql;
import io.github.nextentity.jdbc.JdbcQueryExecutor.QuerySqlBuilder;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

public class SqlServerQuerySqlBuilder implements QuerySqlBuilder {

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
            this(new StringBuilder(), new ArrayList<>(), queryStructure, mappers, new AtomicInteger(), 0);
        }

        @Override
        protected void appendQueryStructure(QueryStructure queryStructure) {
            new Builder(sql, args, queryStructure, mappers, selectIndex, subIndex + 1).doBuilder();
        }

        @Override
        protected void appendOffsetAndLimit() {
            int offset = unwrap(queryStructure.offset());
            int limit = unwrap(queryStructure.limit());
            if (offset > 0 || limit >= 0) {
                if (queryStructure.orderBy() == null || queryStructure.orderBy().isEmpty()) {
                    if (queryStructure.select().distinct()) {
                        sql.append(" order by 1");
                    } else {
                        sql.append(" order by (select 0)");
                    }
                }
                sql.append(" offset ? rows fetch first ? rows only");
                args.add(Math.max(offset, 0));
                args.add(limit < 0 ? Long.MAX_VALUE : limit);
            }
        }

        private static int unwrap(Integer offset) {
            return offset == null ? -1 : offset;
        }

        @Override
        protected void appendOperator(Operator operator) {
            appendOperator(operator == Operator.LENGTH ? "len" : operator.sign());
        }

        @Override
        protected String leftQuotedIdentifier() {
            return "[";
        }

        @Override
        protected String rightQuotedIdentifier() {
            return "]";
        }
    }

}
