package io.github.nextentity.core;

import io.github.nextentity.core.api.Query;
import io.github.nextentity.core.api.expression.QueryStructure;
import io.github.nextentity.core.meta.Metamodel;
import org.jetbrains.annotations.NotNull;

import java.util.List;

public interface QueryExecutor {

    <T> List<T> getList(@NotNull QueryStructure queryStructure);

    Metamodel metamodel();

    default Query createQuery() {
        return createQuery(QueryPostProcessor.NONE);
    }

    default Query createQuery(QueryPostProcessor structurePostProcessor) {
        return new QueryImpl(this, structurePostProcessor);
    }

}
