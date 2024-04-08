package io.github.nextentity.core;

import io.github.nextentity.core.Updaters.UpdateExecutor;
import io.github.nextentity.core.api.Entities;
import io.github.nextentity.core.api.Path;
import io.github.nextentity.core.api.Query;
import io.github.nextentity.core.meta.Metamodel;
import lombok.Getter;

/**
 * @author HuangChengwei
 * @since 2024-04-08 15:12
 */

@Getter
public class EntitiesFactory implements Query {

    private final QueryExecutor queryExecutor;
    private final UpdateExecutor updateExecutor;
    private final QueryPostProcessor queryPostProcessor;
    private final Metamodel metamodel;


    public EntitiesFactory(QueryExecutor queryExecutor,
                           UpdateExecutor updateExecutor,
                           QueryPostProcessor queryPostProcessor,
                           Metamodel metamodel) {
        this.queryExecutor = queryExecutor;
        this.updateExecutor = updateExecutor;
        this.queryPostProcessor = queryPostProcessor;
        this.metamodel = metamodel;
    }

    public <T, ID> Entities<T, ID> getEntities(Class<T> entityType) {
        return new EntitiesImpl<>(this, entityType);
    }

    public <T, ID> Entities<T, ID> getEntities(Class<T> entityType, Path<T, ID> idPath) {
        return new EntitiesImpl<>(this, entityType, idPath);
    }

    @Override
    public <T> Select<T> from(Class<T> type) {
        return getEntities(type);
    }
}
