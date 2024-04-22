package io.github.nextentity.test.db;

import io.github.nextentity.jdbc.ConnectionProvider;

import java.sql.Connection;
import java.sql.SQLException;

public class SingleConnectionProvider implements ConnectionProvider {
    private final Connection connection;

    public SingleConnectionProvider(Connection connection) {
        this.connection = connection;
    }

    @Override
    public <T> T execute(ConnectionCallback<T> action) throws SQLException {
        return action.doInConnection(connection);
    }
}
