package io.github.nextentity.data.jpa;

import io.github.nextentity.api.Repository;
import io.github.nextentity.core.QueryPostProcessor;
import io.github.nextentity.core.RepositoryFactory;
import io.github.nextentity.core.SimpleQueryConfig;
import io.github.nextentity.core.UpdateExecutor;
import io.github.nextentity.core.meta.Metamodel;
import io.github.nextentity.data.EntityTypeUtil;
import io.github.nextentity.data.TransactionalUpdateExecutor;
import io.github.nextentity.jdbc.JdbcQueryExecutor;
import io.github.nextentity.jpa.JpaQueryExecutor;
import io.github.nextentity.jpa.JpaUpdateExecutor;
import jakarta.persistence.EntityManager;
import jakarta.persistence.EntityManagerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.beans.factory.config.DependencyDescriptor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import org.springframework.context.annotation.Scope;
import org.springframework.orm.jpa.SharedEntityManagerCreator;

import java.io.Serializable;

@Configuration
public class JpaRepositoryConfiguration {

    @Bean
    @Scope(ConfigurableBeanFactory.SCOPE_PROTOTYPE)
    @Primary
    protected <T, ID extends Serializable>
    Repository<ID, T> jpaRepository(DependencyDescriptor descriptor,
                                    @Qualifier("jpaRepositoryFactory")
                                    RepositoryFactory factory) {
        Class<T> entityType = EntityTypeUtil.getEntityType(descriptor);
        EntityTypeUtil.checkIdType(descriptor, factory.getMetamodel(), entityType);
        return factory.getRepository(entityType);
    }

    @Bean(name = "jpaRepositoryFactory")
    @Primary
    protected RepositoryFactory jpaEntitiesFactory(JpaQueryExecutor queryExecutor,
                                                   UpdateExecutor updateExecutor,
                                                   @Autowired(required = false)
                                                   QueryPostProcessor queryPostProcessor,
                                                   Metamodel metamodel) {
        return new RepositoryFactory(queryExecutor, updateExecutor, queryPostProcessor, metamodel);
    }

    @Bean
    @Primary
    protected JpaQueryExecutor jpaQueryExecutor(EntityManager entityManager,
                                                Metamodel metamodel,
                                                JdbcQueryExecutor executor) {
        return new JpaQueryExecutor(entityManager, metamodel, executor);
    }

    @Bean("jpaUpdate")
    @Primary
    protected UpdateExecutor jpaUpdateExecutor(EntityManager entityManager,
                                               JpaQueryExecutor jpaQueryExecutor,
                                               Metamodel metamodel,
                                               @Autowired(required = false)
                                               QueryPostProcessor postProcessor) {

        SimpleQueryConfig config = new SimpleQueryConfig()
                .metamodel(metamodel)
                .queryExecutor(jpaQueryExecutor)
                .queryPostProcessor(postProcessor);
        JpaUpdateExecutor jpaUpdate = new JpaUpdateExecutor(entityManager, config);
        return new TransactionalUpdateExecutor(jpaUpdate);
    }

    @Bean
    protected EntityManager entityManager(EntityManagerFactory entityManagerFactory) {
        return SharedEntityManagerCreator.createSharedEntityManager(entityManagerFactory);
    }

}
