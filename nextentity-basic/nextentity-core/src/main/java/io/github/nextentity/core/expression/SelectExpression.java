package io.github.nextentity.core.expression;

import io.github.nextentity.core.api.ExpressionTree;
import lombok.Data;
import lombok.experimental.Accessors;

/**
 * @author HuangChengwei
 * @since 2024/4/17 下午3:24
 */
@Data
@Accessors(fluent = true)
public class SelectExpression implements ExpressionTree, SelectElement {

    private final ExpressionNode rootNode;
    private final Class<?> javaType;

    public static SelectElement of(ExpressionTree expressionTree) {
        return new SelectExpression(expressionTree.rootNode(), Object.class);
    }

}
