package io.github.nextentity.data.jdbc;

import io.github.nextentity.core.QueryStructurePostProcessor;
import io.github.nextentity.core.api.Query;
import io.github.nextentity.core.api.Update;
import io.github.nextentity.core.meta.Metamodel;
import io.github.nextentity.data.common.Access;
import io.github.nextentity.data.common.AccessTypeUtil;
import io.github.nextentity.data.common.Accesses;
import io.github.nextentity.data.common.TransactionalUpdate;
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

@Configuration
public class JdbcAccessConfiguration {

    @Bean
    @Scope(ConfigurableBeanFactory.SCOPE_PROTOTYPE)
    protected <T, ID extends Serializable> Access<T, ID> jdbcAccess(DependencyDescriptor descriptor,
                                                                    @Qualifier("jdbcQuery") Query query,
                                                                    @Qualifier("jdbcUpdate") Update update,
                                                                    Metamodel metamodel) {
        Class<T> entityType = AccessTypeUtil.getEntityType(descriptor);
        Class<?> dependencyType = descriptor.getDependencyType();
        if (Access.class.isAssignableFrom(dependencyType)) {
            AccessTypeUtil.checkIdType(descriptor, metamodel, entityType);
        }
        return Accesses.of(entityType, query, update, metamodel);
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
    protected ResultCollector jdbcResultCollector() {
        return new JdbcResultCollector();
    }

    @Bean
    protected QuerySqlBuilder querySqlBuilder() {
        return new MySqlQuerySqlBuilder();
    }

    @Bean
    protected Update jdbcUpdate(JdbcUpdateSqlBuilder sqlBuilder,
                                ConnectionProvider connectionProvider,
                                Metamodel metamodel) {
        JdbcUpdate jdbcUpdate = new JdbcUpdate(sqlBuilder, connectionProvider, metamodel);
        return new TransactionalUpdate(jdbcUpdate);
    }

    @Bean
    protected JdbcUpdateSqlBuilder jdbcUpdateSqlBuilder() {
        return new MysqlUpdateSqlBuilder();
    }

    @Bean
    protected Query jdbcQuery(JdbcQueryExecutor executor,
                              @Autowired(required = false)
                              QueryStructurePostProcessor structurePostProcessor) {
        return structurePostProcessor != null
                ? executor.createQuery(structurePostProcessor)
                : executor.createQuery();
    }

    @Bean
    @ConditionalOnMissingBean
    protected Metamodel jpaMetamodel() {
        return JpaMetamodel.of();
    }

}
