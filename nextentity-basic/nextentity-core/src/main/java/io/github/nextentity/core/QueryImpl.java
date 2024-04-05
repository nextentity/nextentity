package io.github.nextentity.core;

import io.github.nextentity.core.api.Query;

public class QueryImpl implements Query {
    private final QueryExecutor executor;
    private final QueryStructurePostProcessor structurePostProcessor;

    public QueryImpl(QueryExecutor executor, QueryStructurePostProcessor structurePostProcessor) {
        this.executor = executor;
        this.structurePostProcessor = structurePostProcessor;
    }

    @Override
    public <T> Select<T> from(Class<T> type) {
        return new QueryBuilder<>(executor, type, structurePostProcessor);
    }

    @Override
    public String toString() {
        return "Query[" + executor.getClass().getSimpleName() + "]";
    }
}
