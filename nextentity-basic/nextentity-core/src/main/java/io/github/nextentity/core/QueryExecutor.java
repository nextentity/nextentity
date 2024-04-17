package io.github.nextentity.core;

import io.github.nextentity.core.expression.QueryStructure;
import io.github.nextentity.core.api.Query;
import org.jetbrains.annotations.NotNull;

import java.util.List;

public interface QueryExecutor {

    <T> List<T> getList(@NotNull QueryStructure queryStructure);

    default Query createQuery() {
        return createQuery(QueryPostProcessor.NONE);
    }

    default Query createQuery(QueryPostProcessor structurePostProcessor) {
        return new QueryImpl(this, structurePostProcessor);
    }

}
