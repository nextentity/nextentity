package io.github.nextentity.data.jdbc;

import io.github.nextentity.core.EntitiesFactory;
import io.github.nextentity.core.QueryPostProcessor;
import io.github.nextentity.core.Updaters.UpdateExecutor;
import io.github.nextentity.core.api.Entities;
import io.github.nextentity.core.api.Query;
import io.github.nextentity.core.converter.TypeConverter;
import io.github.nextentity.core.meta.Metamodel;
import io.github.nextentity.data.common.AccessTypeUtil;
import io.github.nextentity.data.common.TransactionalUpdateExecutor;
import io.github.nextentity.jdbc.ConnectionProvider;
import io.github.nextentity.jdbc.JdbcQueryExecutor;
import io.github.nextentity.jdbc.JdbcQueryExecutor.QuerySqlBuilder;
import io.github.nextentity.jdbc.JdbcQueryExecutor.ResultCollector;
import io.github.nextentity.jdbc.JdbcResultCollector;
import io.github.nextentity.jdbc.JdbcUpdate;
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
public class JdbcAccessConfiguration {

    @Bean
    @Scope(ConfigurableBeanFactory.SCOPE_PROTOTYPE)
    protected <T, ID extends Serializable> Entities<T, ID> jdbcAccess(DependencyDescriptor descriptor,
                                                                      @Qualifier("jdbcEntitiesFactory")
                                                                      EntitiesFactory factory) {
        Class<T> entityType = AccessTypeUtil.getEntityType(descriptor);
        AccessTypeUtil.checkIdType(descriptor, factory.getMetamodel(), entityType);
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
        return new ConnectionProvider() {
            @Override
            public <T> T execute(ConnectionCallback<T> action) {
                return jdbcTemplate.execute(action::doInConnection);
            }
        };
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
        JdbcUpdate jdbcUpdate = new JdbcUpdate(sqlBuilder, connectionProvider, metamodel);
        return new TransactionalUpdateExecutor(jdbcUpdate);
    }

    @Bean
    protected JdbcUpdateSqlBuilder jdbcUpdateSqlBuilder() {
        return new MysqlUpdateSqlBuilder();
    }

    @Bean
    protected Query jdbcQuery(JdbcQueryExecutor executor,
                              @Autowired(required = false)
                              QueryPostProcessor structurePostProcessor) {
        return structurePostProcessor != null
                ? executor.createQuery(structurePostProcessor)
                : executor.createQuery();
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
