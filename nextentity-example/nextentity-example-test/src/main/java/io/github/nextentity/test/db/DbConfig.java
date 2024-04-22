package io.github.nextentity.test.db;

import io.github.nextentity.core.RepositoryFactory;
import io.github.nextentity.core.meta.Metamodel;
import io.github.nextentity.jdbc.JdbcQueryExecutor.QuerySqlBuilder;
import io.github.nextentity.jdbc.JdbcUpdateSqlBuilder;
import io.github.nextentity.test.entity.User;
import lombok.Data;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import javax.sql.DataSource;
import java.util.List;

/**
 * @author HuangChengwei
 * @since 2024-04-10 15:37
 */
@Data
public class DbConfig {

    private final RepositoryFactory jdbcFactory;
    private final RepositoryFactory jpaFactory;
    private QuerySqlBuilder querySqlBuilder;
    private JdbcUpdateSqlBuilder updateSqlBuilder;
    private DataSource getDataSource;
    private EntityManagerFactory entityManagerFactory;
    private EntityManager entityManager;
    private List<RepositoryFactory> entitiesFactories;
    private SingleConnectionProvider singleConnectionProvider;
    private Metamodel metamodel;
    private List<User> users;
    private UserRepository jdbc, jpa;
    private final String setPidNullSql;

    public DbConfig(QuerySqlBuilder querySqlBuilder,
                    JdbcUpdateSqlBuilder updateSqlBuilder,
                    DataSource getDataSource,
                    EntityManagerFactory entityManagerFactory,
                    EntityManager entityManager,
                    RepositoryFactory jdbc, RepositoryFactory jpa,
                    List<RepositoryFactory> entitiesFactories,
                    SingleConnectionProvider singleConnectionProvider,
                    Metamodel metamodel, String setPidNullSql) {
        this.querySqlBuilder = querySqlBuilder;
        this.updateSqlBuilder = updateSqlBuilder;
        this.getDataSource = getDataSource;
        this.entityManagerFactory = entityManagerFactory;
        this.entityManager = entityManager;
        this.entitiesFactories = entitiesFactories;
        this.singleConnectionProvider = singleConnectionProvider;
        this.metamodel = metamodel;
        this.setPidNullSql = setPidNullSql;
        this.jdbcFactory = jdbc;
        this.jpaFactory = jpa;
        this.jdbc = new UserRepository(jdbcFactory.getRepository(User.class), this);
        this.jpa = new UserRepository(jpaFactory.getRepository(User.class), this);

        this.users = new DbInitializer(this).initialize();
    }

}
