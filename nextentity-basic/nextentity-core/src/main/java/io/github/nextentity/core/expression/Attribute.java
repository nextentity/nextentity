package io.github.nextentity.core.expression;

import io.github.nextentity.core.api.ExpressionTree.ExpressionNode;
import io.github.nextentity.core.meta.graph.EntityProperty;
import io.github.nextentity.core.meta.graph.EntitySchema;

/**
 * @author HuangChengwei
 * @since 2024/4/17 下午1:27
 */
public interface Attribute extends ExpressionNode, Iterable<String> {
    int deep();

    String get(int i);

    Attribute get(String path);

    Attribute get(Attribute column);

    Attribute parent();

    Attribute subLength(int len);

    EntityProperty toAttribute(EntitySchema entityType);

}
