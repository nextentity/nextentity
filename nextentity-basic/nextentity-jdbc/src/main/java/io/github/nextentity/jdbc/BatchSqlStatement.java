package io.github.nextentity.jdbc;

/**
 * @author HuangChengwei
 * @since 2024/4/22 上午8:42
 */
public class BatchSqlStatement implements SqlStatement {
    private final String sql;
    private final Iterable<? extends Iterable<?>> parameters;

    public BatchSqlStatement(String sql, Iterable<? extends Iterable<?>> parameters) {
        this.sql = sql;
        this.parameters = parameters;
    }

    @Override
    public String sql() {
        return sql;
    }

    @Override
    public Iterable<? extends Iterable<?>> parameters() {
        return parameters;
    }
}
