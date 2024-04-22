package io.github.nextentity.test.db;

import com.mysql.cj.jdbc.MysqlDataSource;
import io.github.nextentity.core.util.Maps;
import io.github.nextentity.jdbc.JdbcQueryExecutor.QuerySqlBuilder;
import io.github.nextentity.jdbc.JdbcUpdateSqlBuilder;
import io.github.nextentity.jdbc.MySqlQuerySqlBuilder;
import io.github.nextentity.jdbc.MysqlUpdateSqlBuilder;
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
 * @since 2024-04-10 10:45
 */
public class Mysql implements DbConfigProvider {

    private final String user = "root";
    private final String password = "root";
    private final String url = "jdbc:mysql:///sql-dsl";


    @Override
    public QuerySqlBuilder querySqlBuilder() {
        return new MySqlQuerySqlBuilder();
    }

    @Override
    public JdbcUpdateSqlBuilder updateSqlBuilder() {
        return new MysqlUpdateSqlBuilder();
    }

    @Override
    public DataSource getDataSource() {
        MysqlDataSource source = new MysqlDataSource();
        source.setUrl(url);
        source.setUser(user);
        source.setPassword(password);
        return source;
    }

    @Override
    public EntityManagerFactory getEntityManagerFactory() {
        Map<String, Object> properties = Maps.<String, Object>hashmap()
                .put(JPA_JDBC_DRIVER, "com.mysql.cj.jdbc.Driver")
                .put(JPA_JDBC_URL, url)
                .put(USER, user)
                .put(PASS, password)
                .put(DIALECT_RESOLVERS, StandardDialectResolver.class.getName())
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
        return "update `user` set pid = null";
    }
}
