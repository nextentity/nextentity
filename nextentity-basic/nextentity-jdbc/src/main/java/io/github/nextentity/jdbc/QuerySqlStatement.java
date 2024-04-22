package io.github.nextentity.jdbc;

import java.util.List;

/**
 * @author HuangChengwei
 * @since 2024/4/16 上午8:42
 */
public class QuerySqlStatement implements SqlStatement {

    private final String sql;

    private final List<?> parameters;

    public QuerySqlStatement(String sql, List<?> parameters) {
        this.sql = sql;
        this.parameters = parameters;
    }

    @Override
    public String toString() {
        return sql;
    }

    public String sql() {
        return sql;
    }

    public Iterable<?> parameters() {
        return parameters;
    }
}
