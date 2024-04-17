package io.github.nextentity.core;

import io.github.nextentity.core.expression.QueryStructure;

public interface QueryPostProcessor {

    QueryPostProcessor NONE = new QueryPostProcessor() {
    };

    default QueryStructure preCountQuery(WhereImpl<?, ?> builder, QueryStructure queryStructure) {
        return queryStructure;
    }

    default QueryStructure preListQuery(WhereImpl<?, ?> builder, QueryStructure queryStructure) {
        return queryStructure;
    }

    default QueryStructure preExistQuery(WhereImpl<?, ?> builder, QueryStructure queryStructure) {
        return queryStructure;
    }

}
