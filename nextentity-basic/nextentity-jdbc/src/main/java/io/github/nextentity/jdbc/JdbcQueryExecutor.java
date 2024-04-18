package io.github.nextentity.jdbc;

import io.github.nextentity.core.QueryExecutor;
import io.github.nextentity.core.SqlLogger;
import io.github.nextentity.core.api.LockModeType;
import io.github.nextentity.core.exception.TransactionRequiredException;
import io.github.nextentity.core.exception.UncheckedSQLException;
import io.github.nextentity.core.expression.QueryStructure;
import io.github.nextentity.core.meta.Metamodel;
import io.github.nextentity.core.meta.graph.EntityProperty;
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
        PreparedSql sql = sqlBuilder.build(queryStructure, metamodel);
        printSql(sql);
        try {
            return connectionProvider.execute(connection -> {
                LockModeType locked = queryStructure.lockType();
                if (locked != null && locked != LockModeType.NONE && connection.getAutoCommit()) {
                    throw new TransactionRequiredException();
                }
                // noinspection SqlSourceToSinkFlow
                try (PreparedStatement statement = connection.prepareStatement(sql.sql())) {
                    JdbcUtil.setParam(statement, sql.args());
                    try (ResultSet resultSet = statement.executeQuery()) {
                        return collector.resolve(resultSet, metamodel, sql.selected(), queryStructure);
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

    private static void printSql(PreparedSql sql) {
        SqlLogger.debug("SQL: {}", sql.sql());
        if (!sql.args().isEmpty()) {
            SqlLogger.debug("ARGS: {}", sql.args());
        }
    }

    public interface QuerySqlBuilder {
        PreparedSql build(QueryStructure structure, Metamodel metamodel);

    }

    public interface PreparedSql {

        String sql();

        List<?> args();

        List<EntityProperty> selected();

    }

    public interface ResultCollector {
        <T> List<T> resolve(
                ResultSet resultSet,
                Metamodel metamodel,
                List<? extends EntityProperty> selected,
                QueryStructure structure) throws SQLException;
    }
}

