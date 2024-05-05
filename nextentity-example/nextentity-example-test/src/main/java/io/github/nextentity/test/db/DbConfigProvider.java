package io.github.nextentity.test.db;

import io.github.nextentity.core.RepositoryFactory;
import io.github.nextentity.core.SimpleQueryConfig;
import io.github.nextentity.core.converter.TypeConverter;
import io.github.nextentity.core.meta.Metamodel;
import io.github.nextentity.core.util.ImmutableList;
import io.github.nextentity.jdbc.JdbcQueryExecutor;
import io.github.nextentity.jdbc.JdbcQueryExecutor.QuerySqlBuilder;
import io.github.nextentity.jdbc.JdbcResultCollector;
import io.github.nextentity.jdbc.JdbcUpdateExecutor;
import io.github.nextentity.jdbc.JdbcUpdateSqlBuilder;
import io.github.nextentity.jpa.JpaNativeQueryExecutor;
import io.github.nextentity.jpa.JpaQueryExecutor;
import io.github.nextentity.jpa.JpaUpdateExecutor;
import io.github.nextentity.meta.jpa.JpaMetamodel;
import jakarta.persistence.EntityManager;
import jakarta.persistence.EntityManagerFactory;
import lombok.SneakyThrows;

import javax.sql.DataSource;
import java.util.List;

/**
 * @author HuangChengwei
 * @since 2024-04-10 10:45
 */
public interface DbConfigProvider {

    @SneakyThrows
    default DbConfig getConfig() {
        EntityManagerFactory entityManagerFactory = getEntityManagerFactory();
        EntityManager manager = entityManagerFactory.createEntityManager();
        DataSource dataSource = getDataSource();
        Metamodel metamodel = JpaMetamodel.of();
        QuerySqlBuilder querySqlBuilder = querySqlBuilder();
        SingleConnectionProvider connectionProvider = new SingleConnectionProvider(dataSource.getConnection());
        JdbcQueryExecutor jdbcQueryExecutor = new JdbcQueryExecutor(metamodel,
                querySqlBuilder,
                connectionProvider,
                new JdbcResultCollector()
        );

        JpaQueryExecutor jpaQueryExecutor = new JpaQueryExecutor(manager, metamodel, jdbcQueryExecutor);
        TypeConverter typeConverter = TypeConverter.ofDefault();
        JpaNativeQueryExecutor jpaNativeQueryExecutor = new JpaNativeQueryExecutor(querySqlBuilder, manager, metamodel, typeConverter);

        SimpleQueryConfig queryConfig = new SimpleQueryConfig()
                .queryExecutor(jpaQueryExecutor)
                .metamodel(metamodel);

        JpaUpdateExecutor jpaUpdateExecutor = new JpaUpdateExecutor(manager, queryConfig);
        JdbcUpdateExecutor jdbcUpdateExecutor = new JdbcUpdateExecutor(updateSqlBuilder(), connectionProvider, metamodel);
        RepositoryFactory jpa = new RepositoryFactory(jpaQueryExecutor, jpaUpdateExecutor, null, metamodel);
        RepositoryFactory jdbc = new RepositoryFactory(jdbcQueryExecutor, jdbcUpdateExecutor, null, metamodel);
        RepositoryFactory jpaNative = new RepositoryFactory(jpaNativeQueryExecutor, jpaUpdateExecutor, null, metamodel);

        List<RepositoryFactory> list = ImmutableList.of(jdbc, jpa, jpaNative);
        return new DbConfig(querySqlBuilder,
                updateSqlBuilder(),
                dataSource,
                entityManagerFactory,
                manager,
                jdbc,
                jpa,
                list,
                connectionProvider,
                metamodel,
                setPidNullSql());
    }

    QuerySqlBuilder querySqlBuilder();

    JdbcUpdateSqlBuilder updateSqlBuilder();

    DataSource getDataSource();

    EntityManagerFactory getEntityManagerFactory();

    String setPidNullSql();

}
