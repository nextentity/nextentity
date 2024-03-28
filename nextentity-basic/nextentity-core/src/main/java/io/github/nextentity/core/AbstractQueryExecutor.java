package io.github.nextentity.core;

import io.github.nextentity.core.api.Query;
import io.github.nextentity.core.api.QueryExecutor;

public interface AbstractQueryExecutor extends QueryExecutor {
    default Query createQuery() {
        return createQuery(QueryStructurePostProcessor.NONE);
    }

    default Query createQuery(QueryStructurePostProcessor structurePostProcessor) {
        return new QueryImpl(this, structurePostProcessor);
    }

}
