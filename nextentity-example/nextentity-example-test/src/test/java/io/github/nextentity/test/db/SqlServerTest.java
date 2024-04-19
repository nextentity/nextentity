package io.github.nextentity.test.db;

import org.junit.jupiter.api.Test;

import java.sql.Connection;
import java.sql.SQLException;

import static org.junit.jupiter.api.Assertions.assertEquals;

class SqlServerTest {
    private final SqlServer sqlServer = new SqlServer();

    // @Test
    // void getDataSource() throws SQLException {
    //     Connection connection = sqlServer.getDataSource().getConnection();
    //     String databaseProductName = connection.getMetaData().getDatabaseProductName();
    //     assertEquals("Microsoft SQL Server", databaseProductName);
    // }
}