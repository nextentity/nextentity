package io.github.nextentity.core.expression;

import io.github.nextentity.core.api.ExpressionTree.ExpressionNode;
import io.github.nextentity.core.meta.Attribute;
import io.github.nextentity.core.meta.EntityType;

/**
 * @author HuangChengwei
 * @since 2024/4/17 下午1:27
 */
public interface PathChain extends ExpressionNode, Iterable<String>, SelectExpression {
    int deep();

    String get(int i);

    PathChain get(String path);

    PathChain get(PathChain column);

    PathChain parent();

    PathChain subLength(int len);

    Attribute toAttribute(EntityType entityType);

}
