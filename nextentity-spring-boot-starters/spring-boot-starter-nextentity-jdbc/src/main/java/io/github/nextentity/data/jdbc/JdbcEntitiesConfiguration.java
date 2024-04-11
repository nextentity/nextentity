package io.github.nextentity.data.jdbc;

import io.github.nextentity.core.EntitiesFactory;
import io.github.nextentity.core.QueryPostProcessor;
import io.github.nextentity.core.Updaters.UpdateExecutor;
import io.github.nextentity.core.api.Entities;
import io.github.nextentity.core.converter.TypeConverter;
import io.github.nextentity.core.meta.Metamodel;
import io.github.nextentity.data.EntityTypeUtil;
import io.github.nextentity.data.TransactionalUpdateExecutor;
import io.github.nextentity.jdbc.ConnectionProvider;
import io.github.nextentity.jdbc.JdbcQueryExecutor;
import io.github.nextentity.jdbc.JdbcQueryExecutor.QuerySqlBuilder;
import io.github.nextentity.jdbc.JdbcQueryExecutor.ResultCollector;
import io.github.nextentity.jdbc.JdbcResultCollector;
import io.github.nextentity.jdbc.JdbcUpdateExecutor;
import io.github.nextentity.jdbc.JdbcUpdateSqlBuilder;
import io.github.nextentity.jdbc.MySqlQuerySqlBuilder;
import io.github.nextentity.jdbc.MysqlUpdateSqlBuilder;
import io.github.nextentity.meta.jpa.JpaMetamodel;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.beans.factory.config.DependencyDescriptor;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Scope;
import org.springframework.jdbc.core.JdbcTemplate;

import java.io.Serializable;
import java.util.List;

@Configuration
public class JdbcEntitiesConfiguration {

    @Bean
    @Scope(ConfigurableBeanFactory.SCOPE_PROTOTYPE)
    protected <T, ID extends Serializable> Entities<ID, T> jdbcEntities(DependencyDescriptor descriptor,
                                                                        @Qualifier("jdbcEntitiesFactory")
                                                                        EntitiesFactory factory) {
        Class<T> entityType = EntityTypeUtil.getEntityType(descriptor);
        EntityTypeUtil.checkIdType(descriptor, factory.getMetamodel(), entityType);
        return factory.getEntities(entityType);
    }

    @Bean(name = "jdbcEntitiesFactory")
    protected EntitiesFactory jdbcEntitiesFactory(JdbcQueryExecutor queryExecutor,
                                                  UpdateExecutor updateExecutor,
                                                  @Autowired(required = false)
                                                  QueryPostProcessor queryPostProcessor,
                                                  Metamodel metamodel) {
        return new EntitiesFactory(queryExecutor, updateExecutor, queryPostProcessor, metamodel);
    }

    @Bean
    protected JdbcQueryExecutor jdbcQueryExecutor(Metamodel metamodel,
                                                  QuerySqlBuilder querySqlBuilder,
                                                  ResultCollector resultCollector,
                                                  ConnectionProvider connectionProvider) {
        return new JdbcQueryExecutor(metamodel, querySqlBuilder, connectionProvider, resultCollector);
    }

    @Bean
    protected ConnectionProvider connectionProvider(JdbcTemplate jdbcTemplate) {
        return new JdbcConnectionProvider(jdbcTemplate);
    }

    @Bean
    protected ResultCollector jdbcResultCollector(List<TypeConverter> typeConverters) {
        return new JdbcResultCollector(TypeConverter.of(typeConverters));
    }

    @Bean
    protected QuerySqlBuilder querySqlBuilder() {
        return new MySqlQuerySqlBuilder();
    }

    @Bean
    protected UpdateExecutor jdbcUpdate(JdbcUpdateSqlBuilder sqlBuilder,
                                        ConnectionProvider connectionProvider,
                                        Metamodel metamodel) {
        JdbcUpdateExecutor jdbcUpdate = new JdbcUpdateExecutor(sqlBuilder, connectionProvider, metamodel);
        return new TransactionalUpdateExecutor(jdbcUpdate);
    }

    @Bean
    protected JdbcUpdateSqlBuilder jdbcUpdateSqlBuilder() {
        return new MysqlUpdateSqlBuilder();
    }

    @Bean
    @ConditionalOnMissingBean
    protected Metamodel jpaMetamodel() {
        return JpaMetamodel.of();
    }

    @Bean
    @ConditionalOnMissingBean
    protected TypeConverter typeConverter() {
        return TypeConverter.ofDefault();
    }

}