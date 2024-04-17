package io.github.nextentity.core.expression;

import io.github.nextentity.core.api.ExpressionTree.ExpressionNode;

/**
 * @author HuangChengwei
 * @since 2024/4/17 下午1:27
 */
public interface Literal extends ExpressionNode {
    Object value();
}
