package io.github.nextentity.core;

import io.github.nextentity.core.api.ExpressionTree.QueryStructure;
import io.github.nextentity.core.api.Query;
import org.jetbrains.annotations.NotNull;

import java.util.List;

public interface QueryExecutor {

    <T> List<T> getList(@NotNull QueryStructure queryStructure);

    default Query createQuery() {
        return createQuery(QueryStructurePostProcessor.NONE);
    }

    default Query createQuery(QueryStructurePostProcessor structurePostProcessor) {
        return new QueryImpl(this, structurePostProcessor);
    }

}
