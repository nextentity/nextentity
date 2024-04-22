package io.github.nextentity.test.db;

import com.microsoft.sqlserver.jdbc.SQLServerDataSource;
import io.github.nextentity.core.util.Maps;
import io.github.nextentity.jdbc.JdbcQueryExecutor.QuerySqlBuilder;
import io.github.nextentity.jdbc.JdbcUpdateSqlBuilder;
import io.github.nextentity.jdbc.SqlServerQuerySqlBuilder;
import io.github.nextentity.jdbc.SqlServerUpdateSqlBuilder;
import org.hibernate.boot.model.naming.CamelCaseToUnderscoresNamingStrategy;
import org.hibernate.engine.jdbc.dialect.internal.StandardDialectResolver;
import org.hibernate.jpa.HibernatePersistenceProvider;
import org.hibernate.tool.schema.Action;

import javax.persistence.EntityManagerFactory;
import javax.sql.DataSource;
import java.util.Map;

import static org.hibernate.cfg.AvailableSettings.*;

/**
 * @author HuangChengwei
 * @since 2024-04-09 15:00
 */
public class SqlServer implements DbConfigProvider {

    static final String url = "jdbc:sqlserver://localhost;encrypt=false;database=nextentity;integratedSecurity=false;";
    static final String username = "sa";
    static final String password = "root";

    public SqlServer() {
    }


    public EntityManagerFactory getEntityManagerFactory() {
        Map<String, Object> properties = Maps.<String, Object>hashmap()
                .put(JPA_JDBC_DRIVER, "com.microsoft.sqlserver.jdbc.SQLServerDriver")
                .put(JPA_JDBC_URL, url)
                .put(USER, username)
                .put(PASS, password)
                .put(DIALECT_RESOLVERS, StandardDialectResolver.class.getName())
                .put(GLOBALLY_QUOTED_IDENTIFIERS, true)
                .put(HBM2DDL_AUTO, Action.UPDATE)
                .put(SHOW_SQL, false)
                .put(FORMAT_SQL, false)
                .put(QUERY_STARTUP_CHECKING, false)
                .put(GENERATE_STATISTICS, false)
                .put(USE_REFLECTION_OPTIMIZER, false)
                .put(USE_SECOND_LEVEL_CACHE, false)
                .put(USE_QUERY_CACHE, false)
                .put(USE_STRUCTURED_CACHE, false)
                .put(STATEMENT_BATCH_SIZE, 2000)
                .put(PHYSICAL_NAMING_STRATEGY, CamelCaseToUnderscoresNamingStrategy.class)
                .build();
        return new HibernatePersistenceProvider()
                .createContainerEntityManagerFactory(new HibernateUnitInfo(), properties);
    }

    @Override
    public String setPidNullSql() {
        return "update [user] set pid = null";
    }

    public DataSource getDataSource() {
        SQLServerDataSource source = new SQLServerDataSource();
        source.setURL(url);
        source.setUser(username);
        source.setPassword(password);
        return source;
    }

    @Override
    public QuerySqlBuilder querySqlBuilder() {
        return new SqlServerQuerySqlBuilder();
    }

    @Override
    public JdbcUpdateSqlBuilder updateSqlBuilder() {
        return new SqlServerUpdateSqlBuilder();
    }

}
