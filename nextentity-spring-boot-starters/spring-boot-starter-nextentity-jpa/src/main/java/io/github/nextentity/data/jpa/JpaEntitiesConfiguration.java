package io.github.nextentity.data.jpa;

import io.github.nextentity.core.EntitiesFactory;
import io.github.nextentity.core.QueryPostProcessor;
import io.github.nextentity.core.Updaters.UpdateExecutor;
import io.github.nextentity.core.api.Entities;
import io.github.nextentity.core.meta.Metamodel;
import io.github.nextentity.data.EntityTypeUtil;
import io.github.nextentity.data.TransactionalUpdateExecutor;
import io.github.nextentity.jdbc.JdbcQueryExecutor;
import io.github.nextentity.jpa.JpaQueryExecutor;
import io.github.nextentity.jpa.JpaUpdate;
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
public class JpaEntitiesConfiguration {

    @Bean
    @Primary
    @Scope(ConfigurableBeanFactory.SCOPE_PROTOTYPE)
    protected <T, ID extends Serializable> Entities<T, ID> jpaEntities(DependencyDescriptor descriptor,
                                                                       @Qualifier("jpaEntitiesFactory")
                                                                       EntitiesFactory factory) {
        Class<T> entityType = EntityTypeUtil.getEntityType(descriptor);
        EntityTypeUtil.checkIdType(descriptor, factory.getMetamodel(), entityType);
        return factory.getEntities(entityType);
    }

    @Bean(name = "jpaEntitiesFactory")
    @Primary
    protected EntitiesFactory jpaEntitiesFactory(JpaQueryExecutor queryExecutor,
                                                 UpdateExecutor updateExecutor,
                                                 @Autowired(required = false)
                                                 QueryPostProcessor queryPostProcessor,
                                                 Metamodel metamodel) {
        return new EntitiesFactory(queryExecutor, updateExecutor, queryPostProcessor, metamodel);
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
    protected UpdateExecutor jpaUpdateExecutor(EntityManager entityManager, JpaQueryExecutor jpaQueryExecutor) {
        JpaUpdate jpaUpdate = new JpaUpdate(entityManager, jpaQueryExecutor);
        return new TransactionalUpdateExecutor(jpaUpdate);
    }

    @Bean
    protected EntityManager entityManager(EntityManagerFactory entityManagerFactory) {
        return SharedEntityManagerCreator.createSharedEntityManager(entityManagerFactory);
    }

}
