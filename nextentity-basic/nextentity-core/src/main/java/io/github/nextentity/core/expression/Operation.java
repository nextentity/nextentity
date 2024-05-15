package io.github.nextentity.core.expression;

import io.github.nextentity.api.Expression;

import java.util.List;

/**
 * @author HuangChengwei
 * @since 2024/4/17 下午1:27
 */
public interface Operation extends Expression {
    List<? extends Expression> operands();

    Operator operator();

    default Expression operand(int index) {
        List<? extends Expression> args = operands();
        return args == null || args.size() < (index + 1) ? null : args.get(index);
    }

    default Expression firstOperand() {
        return operand(0);
    }

    default Expression secondOperand() {
        return operand(1);
    }

    default Expression thirdOperand() {
        return operand(2);
    }
}
