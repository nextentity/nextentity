package io.github.nextentity.data.jpa;

import io.github.nextentity.core.QueryStructurePostProcessor;
import io.github.nextentity.core.api.Query;
import io.github.nextentity.core.api.Update;
import io.github.nextentity.core.meta.Metamodel;
import io.github.nextentity.data.common.Access;
import io.github.nextentity.data.common.Accesses;
import io.github.nextentity.data.common.TransactionalUpdate;
import io.github.nextentity.jdbc.JdbcQueryExecutor;
import io.github.nextentity.jpa.JpaQueryExecutor;
import io.github.nextentity.jpa.JpaUpdate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.beans.factory.config.DependencyDescriptor;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import org.springframework.context.annotation.Scope;
import org.springframework.orm.jpa.SharedEntityManagerCreator;

import javax.persistence.EntityManager;
import javax.persistence.EntityManagerFactory;
import java.io.Serializable;

@Configuration
public class JpaAccessConfiguration {
    @Bean
    @Primary
    @Scope(ConfigurableBeanFactory.SCOPE_PROTOTYPE)
    protected <T, ID extends Serializable> Access<T, ID> jpaAccess(DependencyDescriptor descriptor,
                                                                   @Qualifier("jpaQuery") Query query,
                                                                   @Qualifier("jpaUpdate") Update update,
                                                                   Metamodel metamodel) {
        return Accesses.of(descriptor, query, update, metamodel);
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
    protected Update jpaUpdate(EntityManager entityManager, JpaQueryExecutor jpaQueryExecutor) {
        JpaUpdate jpaUpdate = new JpaUpdate(entityManager, jpaQueryExecutor);
        return new TransactionalUpdate(jpaUpdate);
    }

    @Bean
    protected EntityManager entityManager(EntityManagerFactory entityManagerFactory) {
        return SharedEntityManagerCreator.createSharedEntityManager(entityManagerFactory);
    }

    @Bean
    @Primary
    protected Query jpaQuery(JpaQueryExecutor executor,
                             @Autowired(required = false)
                             QueryStructurePostProcessor structurePostProcessor) {
        return structurePostProcessor != null
                ? executor.createQuery(structurePostProcessor)
                : executor.createQuery();
    }

}
