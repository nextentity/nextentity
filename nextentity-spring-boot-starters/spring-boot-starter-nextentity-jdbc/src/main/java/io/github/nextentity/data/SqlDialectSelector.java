package io.github.nextentity.data;

import io.github.nextentity.core.expression.QueryStructure;
import io.github.nextentity.core.meta.Metamodel;
import io.github.nextentity.core.meta.graph.EntityProperty;
import io.github.nextentity.core.meta.graph.EntitySchema;
import io.github.nextentity.jdbc.JdbcQueryExecutor;
import io.github.nextentity.jdbc.JdbcQueryExecutor.QuerySqlBuilder;
import io.github.nextentity.jdbc.JdbcUpdateSqlBuilder;
import io.github.nextentity.jdbc.MySqlQuerySqlBuilder;
import io.github.nextentity.jdbc.MysqlUpdateSqlBuilder;
import io.github.nextentity.jdbc.SqlServerQuerySqlBuilder;
import io.github.nextentity.jdbc.SqlServerUpdateSqlBuilder;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.factory.InitializingBean;

import javax.sql.DataSource;
import java.sql.DatabaseMetaData;
import java.sql.SQLException;
import java.util.List;
import java.util.Objects;

/**
 * @author HuangChengwei
 * @since 2024/4/11 下午3:35
 */
public class SqlDialectSelector implements QuerySqlBuilder, JdbcUpdateSqlBuilder, InitializingBean {

    private QuerySqlBuilder querySqlBuilder;
    private JdbcUpdateSqlBuilder updateSqlBuilder;

    public SqlDialectSelector setByDataSource(DataSource dataSource) throws SQLException {
        DatabaseMetaData metaData = dataSource.getConnection().getMetaData();
        String driverName = metaData.getDriverName().toLowerCase();
        if (driverName.contains("mysql") || driverName.contains("maria")) {
            querySqlBuilder = new MySqlQuerySqlBuilder();
            updateSqlBuilder = new MysqlUpdateSqlBuilder();
        } else if (driverName.contains("mssql") || driverName.contains("sql server")) {
            querySqlBuilder = new SqlServerQuerySqlBuilder();
            updateSqlBuilder = new SqlServerUpdateSqlBuilder();
        }
        return this;
    }

    @Override
    public void afterPropertiesSet() {
        Objects.requireNonNull(querySqlBuilder);
        Objects.requireNonNull(updateSqlBuilder);
    }


    @Override
    public JdbcQueryExecutor.PreparedSql build(QueryStructure structure, Metamodel metamodel) {
        return querySqlBuilder.build(structure, metamodel);
    }

    @Override
    public InsertSql buildInsert(Iterable<?> entities, @NotNull EntitySchema entityType) {
        return updateSqlBuilder.buildInsert(entities, entityType);
    }

    @Override
    public PreparedSql buildUpdate(@NotNull EntitySchema entityType) {
        return updateSqlBuilder.buildUpdate(entityType);
    }

    @Override
    public PreparedSql buildUpdate(@NotNull EntitySchema entityType, @NotNull List<EntityProperty> columns) {
        return updateSqlBuilder.buildUpdate(entityType, columns);
    }

    @Override
    public PreparedSql buildDelete(EntitySchema entity) {
        return updateSqlBuilder.buildDelete(entity);
    }
}
