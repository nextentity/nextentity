package io.github.nextentity.jdbc;

import io.github.nextentity.core.SqlLogger;

/**
 * @author HuangChengwei
 * @since 2024/4/22 上午8:37
 */
public interface SqlStatement {
    String sql();

    Iterable<?> parameters();

    default void print() {
        SqlLogger.debug(sql());
        SqlLogger.debug("sql parameters:{}", parameters());
    }

}
