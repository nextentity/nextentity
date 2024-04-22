package io.github.nextentity.jdbc;

/**
 * @author HuangChengwei
 * @since 2024/4/22 上午8:30
 */
public class InsertSqlStatement extends BatchSqlStatement implements SqlStatement {
    private final Iterable<?> entities;
    private final boolean returnGeneratedKeys;

    public InsertSqlStatement(Iterable<?> entities, String sql, Iterable<? extends Iterable<?>> parameters, boolean returnGeneratedKeys) {
        super(sql, parameters);
        this.entities = entities;
        this.returnGeneratedKeys = returnGeneratedKeys;
    }

    public boolean returnGeneratedKeys() {
        return returnGeneratedKeys;
    }

    public Iterable<?> entities() {
        return entities;
    }
}
