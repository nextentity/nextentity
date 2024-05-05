package io.github.nextentity.core;

import io.github.nextentity.core.meta.Metamodel;

/**
 * @author HuangChengwei
 * @since 2024-05-06 13:37
 */
public interface QueryConfig {

    Metamodel metamodel();

    QueryExecutor queryExecutor();

    QueryPostProcessor queryPostProcessor();

}
