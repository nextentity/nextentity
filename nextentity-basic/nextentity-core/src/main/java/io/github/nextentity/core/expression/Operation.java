package io.github.nextentity.core.expression;

import io.github.nextentity.core.api.ExpressionTree.ExpressionNode;
import io.github.nextentity.core.api.Operator;

import java.util.List;

/**
 * @author HuangChengwei
 * @since 2024/4/17 下午1:27
 */
public interface Operation extends ExpressionNode {
    List<? extends ExpressionNode> operands();

    Operator operator();

    default ExpressionNode operand(int index) {
        List<? extends ExpressionNode> args = operands();
        return args == null || args.size() < (index + 1) ? null : args.get(index);
    }

    default ExpressionNode firstOperand() {
        return operand(0);
    }

    default ExpressionNode secondOperand() {
        return operand(1);
    }

    default ExpressionNode thirdOperand() {
        return operand(2);
    }
}
