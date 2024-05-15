package io.github.nextentity.core.expression;

import io.github.nextentity.api.Expression;

/**
 * @author HuangChengwei
 * @since 2024/4/17 下午1:27
 */
public interface Literal extends Expression {
    Object value();
}
