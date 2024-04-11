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

        public Builder(StringBuilder sql, List<Object> args, QueryStructure queryStructure, Metamodel mappers, AtomicInteger selectIndex, int subIndex) {
            super(sql, args, queryStructure, mappers, selectIndex, subIndex);
        }

        public Builder(QueryStructure queryStructure, Metamodel mappers) {
            this(new StringBuilder(), new ArrayList<>(), queryStructure, mappers, new AtomicInteger(), 0);
        }

        protected void doBuilder() {
            boolean distinctAndHasLimit = distinctAndHasLimit();
            if (distinctAndHasLimit) {
                sql.append(SELECT).append("* from (");
            }
            appendSelect();
            appendFrom();
            int joinIndex = sql.length();
            appendWhere();
            appendGroupBy();
            appendOrderBy();
            appendHaving();
            insertJoin(joinIndex);
            if (distinctAndHasLimit) {
                sql.append(") ").append(fromAlias).append("dst");
            }
            appendOffsetAndLimit(distinctAndHasLimit || noOrderBy());
            appendLockModeType(queryStructure.lockType());
        }

        @Override
        protected void appendQueryStructure(QueryStructure queryStructure) {
            new Builder(sql, args, queryStructure, mappers, selectIndex, subIndex + 1).doBuilder();
        }

        @Override
        protected void appendOffsetAndLimit() {

        }

        private boolean noOrderBy() {
            return queryStructure.orderBy().isEmpty();
        }

        private boolean distinctAndHasLimit() {
            return queryStructure.select().distinct() &&
                   (unwrap(queryStructure.offset()) > 0 || unwrap(queryStructure.limit()) >= 0);
        }


        private static int unwrap(Integer offset) {
            return offset == null ? -1 : offset;
        }

        protected void appendOffsetAndLimit(boolean appendOrderBy) {
            int offset = unwrap(queryStructure.offset());
            int limit = unwrap(queryStructure.limit());
            if (offset > 0 || limit >= 0) {
                if (appendOrderBy) {
                    sql.append(" order by (select 0)");
                }
                sql.append(" offset ? rows fetch first ? rows only");
                args.add(Math.max(offset, 0));
                args.add(limit < 0 ? Long.MAX_VALUE : limit);
            }
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
