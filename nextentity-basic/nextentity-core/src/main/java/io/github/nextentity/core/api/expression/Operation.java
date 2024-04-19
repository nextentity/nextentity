package io.github.nextentity.core.api.expression;

import io.github.nextentity.core.api.Operator;

import java.util.List;

/**
 * @author HuangChengwei
 * @since 2024/4/17 下午1:27
 */
public interface Operation extends BaseExpression {
    List<? extends BaseExpression> operands();

    Operator operator();

    default BaseExpression operand(int index) {
        List<? extends BaseExpression> args = operands();
        return args == null || args.size() < (index + 1) ? null : args.get(index);
    }

    default BaseExpression firstOperand() {
        return operand(0);
    }

    default BaseExpression secondOperand() {
        return operand(1);
    }

    default BaseExpression thirdOperand() {
        return operand(2);
    }
}
