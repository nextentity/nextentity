package io.github.nextentity.core.expression;

import io.github.nextentity.core.api.ExpressionTree;

/**
 * @author HuangChengwei
 * @since 2024/4/17 下午3:24
 */
public interface SelectExpression extends ExpressionTree, SelectElement {

    static SelectExpression of(ExpressionTree expressionTree) {
        return (SelectExpression) expressionTree;
    }

}
