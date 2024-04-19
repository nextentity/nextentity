package io.github.nextentity.core;

import lombok.Getter;

import java.util.List;

/**
 * @author HuangChengwei
 * @since 2024/4/16 上午8:42
 */
@Getter
public class SqlStatement<T> {

    protected String sql;

    protected List<T> parameters;

    public SqlStatement(String sql, List<T> parameters) {
        this.sql = sql;
        this.parameters = parameters;
    }

    @Override
    public String toString() {
        return sql;
    }
}
