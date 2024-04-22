package io.github.nextentity.jdbc;

import org.jetbrains.annotations.NotNull;

public class MysqlUpdateSqlBuilder extends AbstractUpdateSqlBuilder {

    @Override
    protected @NotNull String rightTicks() {
        return "`";
    }

    @Override
    protected @NotNull String leftTicks() {
        return "`";
    }
}
