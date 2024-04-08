package io.github.nextentity.core.api;

import io.github.nextentity.core.util.tuple.Tuple;

import java.io.Serializable;
import java.util.List;

public interface ExpressionTree extends Serializable {
    ExpressionNode rootNode();

    sealed interface ExpressionNode extends Serializable, ExpressionTree permits Literal, Column, Operation, QueryStructure {
        @Override
        default ExpressionNode rootNode() {
            return this;
        }

    }

    non-sealed interface Column extends ExpressionNode, Iterable<String> {
        int size();

        String get(int i);

        ExpressionTree.Column get(String path);

        ExpressionTree.Column get(ExpressionTree.Column column);

        ExpressionTree.Column parent();

        ExpressionTree.Column subLength(int len);
    }

    non-sealed interface Literal extends ExpressionNode {
        Object value();
    }

    non-sealed interface Operation extends ExpressionNode {
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

    non-sealed interface QueryStructure extends ExpressionNode {

        Selection select();

        From from();

        ExpressionNode where();

        List<? extends ExpressionNode> groupBy();

        List<? extends Order<?>> orderBy();

        ExpressionNode having();

        Integer offset();

        Integer limit();

        LockModeType lockType();

        List<? extends Column> fetch();

        interface Selection extends Serializable {

            Class<?> resultType();

            boolean distinct();

            interface MultiSelected extends Selection {
                List<? extends ExpressionNode> expressions();

                @Override
                default Class<?> resultType() {
                    return Tuple.class;
                }

            }

            interface SingleSelected extends Selection {
                ExpressionNode expression();

            }

            interface ProjectionSelected extends Selection {
            }

            interface EntitySelected extends Selection {
            }

        }

        interface From extends Serializable {

            Class<?> type();

            interface Entity extends From {

            }

            interface FromSubQuery extends From, ExpressionTree.QueryStructure {
                @Override
                default Class<?> type() {
                    return select().resultType();
                }
            }

        }

        interface Order<T> extends Serializable {

            ExpressionNode expression();

            SortOrder order();

        }

    }
}
