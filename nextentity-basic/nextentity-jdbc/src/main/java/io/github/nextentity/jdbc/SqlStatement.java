package io.github.nextentity.jdbc;

/**
 * @author HuangChengwei
 * @since 2024/4/22 上午8:37
 */
public interface SqlStatement {
    String sql();

    Iterable<?> parameters();
}
