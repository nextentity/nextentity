package io.github.nextentity.test.db;

import io.github.nextentity.core.EntitiesFaced;
import io.github.nextentity.core.api.Entities;
import io.github.nextentity.test.entity.User;
import lombok.SneakyThrows;

import java.sql.Connection;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Function;

/**
 * @author HuangChengwei
 * @since 2024-04-10 16:10
 */
public class UserEntities extends EntitiesFaced<Integer, User> {
    private final DbConfig dbConfig;
    private final Transaction transaction;

    public UserEntities(Entities<Integer, User> target, DbConfig dbConfig) {
        super(target);
        this.dbConfig = dbConfig;
        this.transaction = new Transaction(dbConfig);
    }

    public DbConfig getConfig() {
        return dbConfig;
    }

    public void doInTransaction(Runnable action) {
        transaction.doInTransaction(action);
    }

    public void doInTransaction(Consumer<Connection> action) {
        transaction.doInTransaction(action);
    }

    @SneakyThrows
    public <T> T doInTransaction(Function<Connection, T> action) {
        return transaction.doInTransaction(action);
    }

    public List<User> users() {
        return dbConfig.getUsers();
    }
}
