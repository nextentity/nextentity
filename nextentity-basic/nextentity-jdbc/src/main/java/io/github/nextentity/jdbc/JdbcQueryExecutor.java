package io.github.nextentity.jdbc;

import io.github.nextentity.core.QueryExecutor;
import io.github.nextentity.core.SqlLogger;
import io.github.nextentity.core.SqlStatement;
import io.github.nextentity.core.api.LockModeType;
import io.github.nextentity.core.exception.TransactionRequiredException;
import io.github.nextentity.core.exception.UncheckedSQLException;
import io.github.nextentity.core.expression.QueryStructure;
import io.github.nextentity.core.meta.Metamodel;
import lombok.extern.slf4j.Slf4j;
import org.jetbrains.annotations.NotNull;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;

@Slf4j
public class JdbcQueryExecutor implements QueryExecutor {

    @NotNull
    private final Metamodel metamodel;
    @NotNull
    private final QuerySqlBuilder sqlBuilder;
    @NotNull
    private final ConnectionProvider connectionProvider;
    @NotNull
    private final ResultCollector collector;

    public JdbcQueryExecutor(@NotNull Metamodel metamodel,
                             @NotNull QuerySqlBuilder sqlBuilder,
                             @NotNull ConnectionProvider connectionProvider,
                             @NotNull ResultCollector collector) {
        this.metamodel = metamodel;
        this.sqlBuilder = sqlBuilder;
        this.connectionProvider = connectionProvider;
        this.collector = collector;
    }

    @Override
    @NotNull
    public <R> List<R> getList(@NotNull QueryStructure queryStructure) {
        SqlStatement<?> sql = sqlBuilder.build(queryStructure, metamodel);
        printSql(sql);
        try {
            return connectionProvider.execute(connection -> {
                LockModeType locked = queryStructure.lockType();
                if (locked != null && locked != LockModeType.NONE && connection.getAutoCommit()) {
                    throw new TransactionRequiredException();
                }
                // noinspection SqlSourceToSinkFlow
                try (PreparedStatement statement = connection.prepareStatement(sql.getSql())) {
                    JdbcUtil.setParam(statement, sql.getParameters());
                    try (ResultSet resultSet = statement.executeQuery()) {
                        return collector.resolve(resultSet, metamodel, queryStructure);
                    }
                }
            });
        } catch (SQLException e) {
            throw new UncheckedSQLException(e);
        }
    }

    @Override
    public Metamodel metamodel() {
        return metamodel;
    }

    private static void printSql(SqlStatement<?> sql) {
        SqlLogger.debug("SQL: {}", sql.getSql());
        if (!sql.getParameters().isEmpty()) {
            SqlLogger.debug("ARGS: {}", sql.getParameters());
        }
    }

    public interface QuerySqlBuilder {
        SqlStatement<?> build(QueryStructure structure, Metamodel metamodel);
    }



    public interface ResultCollector {
        <T> List<T> resolve(
                ResultSet resultSet,
                Metamodel metamodel,
                QueryStructure structure) throws SQLException;
    }
}

