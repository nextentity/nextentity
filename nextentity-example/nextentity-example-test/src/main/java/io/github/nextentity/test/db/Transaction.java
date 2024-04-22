package io.github.nextentity.test.db;

import io.github.nextentity.core.exception.UncheckedSQLException;
import io.github.nextentity.core.util.Exceptions;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;

import javax.persistence.EntityTransaction;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.function.Consumer;
import java.util.function.Function;

/**
 * @author HuangChengwei
 * @since 2024-04-10 16:19
 */
@Slf4j
public class Transaction {
    protected DbConfig config;

    public Transaction() {
    }

    public Transaction(DbConfig config) {
        this.config = config;
    }

    public void doInTransaction(Consumer<Connection> action) {
        Object o = doInTransaction(connection -> {
            action.accept(connection);
            return null;
        });
        if (o != null) {
            log.trace("{}", o);
        }
    }


    @SneakyThrows
    public <T> T doInTransaction(Function<Connection, T> action) {
        return config.getSingleConnectionProvider().execute(connection -> {
            T result;
            boolean autoCommit = connection.getAutoCommit();
            try {
                if (autoCommit) {
                    connection.setAutoCommit(false);
                }
                result = action.apply(connection);
                connection.commit();
            } catch (Throwable e) {
                connection.rollback();
                throw Exceptions.sneakyThrow(e);
            } finally {
                if (autoCommit) {
                    connection.setAutoCommit(true);
                }
            }

            return result;
        });
    }

    public void doInTransaction(Runnable action) {
        try {
            executeAction(action);
        } catch (SQLException e) {
            throw new UncheckedSQLException(e);
        }
    }

    private void executeAction(Runnable action) throws SQLException {
        EntityTransaction transaction = config.getEntityManager().getTransaction();
        boolean rollback = false;
        SingleConnectionProvider provider = config.getSingleConnectionProvider();
        provider.execute(connection -> {
            connection.setAutoCommit(false);
            return null;
        });
        transaction.begin();
        try {
            action.run();
        } catch (Throwable throwable) {
            rollback = true;
            provider.execute(connection -> {
                connection.rollback();
                return null;
            });
            transaction.rollback();
            throw throwable;
        } finally {
            if (!rollback) {
                provider.execute(connection -> {
                    connection.commit();
                    return null;
                });
                transaction.commit();
            }
        }
    }

}
