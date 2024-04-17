package io.github.nextentity.core;

import lombok.Getter;

import java.util.List;

/**
 * @author HuangChengwei
 * @since 2024/4/16 上午8:42
 */
@Getter
public class SqlStatement {

    protected String sql;

    protected List<List<?>> parameters;

    public SqlStatement(String sql, List<List<?>> parameters) {
        this.sql = sql;
        this.parameters = parameters;
    }

    @Override
    public String toString() {
        return sql;
    }
}
