package io.github.nextentity.core;

import io.github.nextentity.core.Updaters.UpdateExecutor;
import io.github.nextentity.core.api.Entities;
import io.github.nextentity.core.meta.Metamodel;

import java.io.Serializable;

/**
 * @author HuangChengwei
 * @since 2024/4/15 上午9:00
 */
public class RepositoryFactory extends EntitiesFactory {

    public RepositoryFactory(QueryExecutor queryExecutor,
                             UpdateExecutor updateExecutor,
                             QueryPostProcessor queryPostProcessor,
                             Metamodel metamodel) {
        super(queryExecutor, updateExecutor, queryPostProcessor, metamodel);
    }

    public <ID extends Serializable, T extends Persistable<ID>> Repository<ID, T>
    getRepository(Class<T> entityType) {
        return new RepositoryImpl<>(getEntities(entityType, Persistable::getId));
    }

    private static class RepositoryImpl<ID extends Serializable, T extends Persistable<ID>>
            extends EntitiesFaced<ID, T>
            implements Repository<ID, T> {
        public RepositoryImpl(Entities<ID, T> target) {
            super(target);
        }
    }

}
