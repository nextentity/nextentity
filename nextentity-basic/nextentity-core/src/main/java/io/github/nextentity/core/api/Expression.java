package io.github.nextentity.core.api;

import io.github.nextentity.core.util.tuple.Tuple;

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

        ExpressionTree where();

        List<? extends ExpressionTree> groupBy();

        List<? extends Order<?>> orderBy();

        ExpressionTree having();

        Integer offset();

        Integer limit();

        LockModeType lockType();

        List<? extends Column> fetch();
    }

    interface From extends Serializable {

        Class<?> type();

        interface Entity extends From {

        }

        interface FromSubQuery extends From, QueryStructure {
            @Override
            default Class<?> type() {
                return select().resultType();
            }
        }

    }

    @SuppressWarnings("unused")
    interface Order<T> extends Serializable {

        ExpressionTree expression();

        SortOrder order();

    }

    interface Selection extends Serializable {

        Class<?> resultType();

        boolean distinct();

        interface MultiSelected extends Selection {
            List<? extends ExpressionTree> expressions();

            @Override
            default Class<?> resultType() {
                return Tuple.class;
            }

        }

        interface SingleSelected extends Selection {
            ExpressionTree expression();

        }

        interface ProjectionSelected extends Selection {
        }

        interface EntitySelected extends Selection {
        }

    }
}
