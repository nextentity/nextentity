package io.github.nextentity.core.api;

import java.io.Serializable;
import java.util.List;

public interface Expression extends Serializable {
    ExpressionTree tree();

    sealed interface ExpressionTree extends Serializable, Expression permits Constant, Column, Operation, QueryStructure {
        @Override
        default ExpressionTree tree() {
            return this;
        }
    }

    non-sealed interface Column extends ExpressionTree, Iterable<String> {
        int size();

        String get(int i);

        Column get(String path);

        Column get(Column column);

        Column parent();

        Column subLength(int len);
    }

    non-sealed interface Constant extends ExpressionTree {
        Object value();
    }

    non-sealed interface Operation extends ExpressionTree {
        List<? extends ExpressionTree> operands();

        Operator operator();

        default ExpressionTree operand(int index) {
            List<? extends ExpressionTree> args = operands();
            return args == null || args.size() < (index + 1) ? null : args.get(index);
        }

        default ExpressionTree firstOperand() {
            return operand(0);
        }

        default ExpressionTree secondOperand() {
            return operand(1);
        }

        default ExpressionTree thirdOperand() {
            return operand(2);
        }
    }

    non-sealed interface QueryStructure extends ExpressionTree {

        Selection select();

        From from();

        Expression where();

        List<? extends Expression> groupBy();

        List<? extends Order<?>> orderBy();

        Expression having();

        Integer offset();

        Integer limit();

        LockModeType lockType();

        List<? extends Column> fetch();
    }
}
