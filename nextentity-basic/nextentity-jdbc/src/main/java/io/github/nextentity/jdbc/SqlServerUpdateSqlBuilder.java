package io.github.nextentity.jdbc;

import org.jetbrains.annotations.NotNull;

public class SqlServerUpdateSqlBuilder extends AbstractJdbcUpdateSqlBuilder {

    @Override
    protected @NotNull String leftTicks() {
        return "[";
    }

    @Override
    protected @NotNull String rightTicks() {
        return "]";
    }
}
