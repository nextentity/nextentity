package io.github.nextentity.core;

import io.github.nextentity.core.meta.Metamodel;
import lombok.Getter;
import lombok.Setter;
import lombok.experimental.Accessors;

/**
 * @author HuangChengwei
 * @since 2024-05-06 13:59
 */
@Getter
@Setter
@Accessors(chain = true, fluent = true)
public class SimpleQueryConfig implements QueryConfig {
    private QueryExecutor queryExecutor;
    private Metamodel metamodel;
    private QueryPostProcessor queryPostProcessor = QueryPostProcessor.NONE;
}
