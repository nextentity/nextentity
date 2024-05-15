package io.github.nextentity.jdbc;

import io.github.nextentity.api.Expression;
import io.github.nextentity.jdbc.JdbcQueryExecutor.QuerySqlBuilder;

import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

public class MySqlQuerySqlBuilder implements QuerySqlBuilder {

    @Override
    public QuerySqlStatement build(QueryContext context) {
        return new Builder(context).build();
    }

    static class Builder extends AbstractQuerySqlBuilder {

        public Builder(StringBuilder sql,
                       List<Object> args,
                       QueryContext context,
                       AtomicInteger selectIndex,
                       int subIndex) {
            super(sql, args, context, selectIndex, subIndex);
        }


        public Builder(QueryContext context) {
            super(context);
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
        protected void appendQueryStructure(QueryContext subContext) {
            new Builder(sql, args, subContext, selectIndex, subIndex + 1).doBuilder();
        }

        @Override
        protected void appendPredicate(Expression node) {
            appendExpression(node);
        }

        @Override
        protected void appendOffsetAndLimit() {
            int offset = unwrap(context.getStructure().offset());
            int limit = unwrap(context.getStructure().limit());
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

}
