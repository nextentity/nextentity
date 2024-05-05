package io.github.nextentity.core;

import io.github.nextentity.core.expression.QueryStructure;

public interface QueryPostProcessor {

    QueryPostProcessor NONE = new QueryPostProcessor() {
    };

    default QueryStructure preCountQuery(QueryStructure queryStructure) {
        return queryStructure;
    }

    default QueryStructure preListQuery(QueryStructure queryStructure) {
        return queryStructure;
    }

    default QueryStructure preExistQuery(QueryStructure queryStructure) {
        return queryStructure;
    }

}
