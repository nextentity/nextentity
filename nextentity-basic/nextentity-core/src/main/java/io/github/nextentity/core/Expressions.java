package io.github.nextentity.core;

import io.github.nextentity.core.api.Expression;
import io.github.nextentity.core.api.Expression.Column;
import io.github.nextentity.core.api.Expression.Constant;
import io.github.nextentity.core.api.Expression.ExpressionTree;
import io.github.nextentity.core.api.Expression.Operation;
import io.github.nextentity.core.api.Lists;
import io.github.nextentity.core.api.Operator;
import io.github.nextentity.core.api.Path;
import io.github.nextentity.core.api.TypedExpression.PathExpression;
import io.github.nextentity.core.util.Paths;
import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@Slf4j
public class Expressions {

    public static final ExpressionTree TRUE = of(true);
    public static final ExpressionTree FALSE = of(false);

    public static boolean isNullOrTrue(Expression expression) {
        return expression == null || Expressions.isTrue(expression);
    }

    public static boolean isTrue(Expression expression) {
        ExpressionTree tree = expression.tree();
        return tree instanceof Constant
               && Boolean.TRUE.equals(((Constant) tree).value());
    }

    public static boolean isFalse(Expression expression) {
        ExpressionTree tree = expression.tree();
        return tree instanceof Constant
               && Boolean.FALSE.equals(((Constant) tree).value());
    }

    public static ExpressionTree of(Object value) {
        if (value instanceof Expression) {
            return ((Expression) value).tree();
        } else if (value instanceof Path<?, ?>) {
            return of((Path<?, ?>) value);
        }
        return ExpressionTrees.newConstant(value);
    }

    public static Column of(Path<?, ?> path) {
        String property = columnName(path);
        return column(property);
    }

    public static String columnName(Path<?, ?> path) {
        return PathReference.of(path).getPropertyName();
    }

    public static Column column(String path) {
        List<String> paths = new ArrayList<>(1);
        paths.add(path);
        return column(paths);
    }

    public static Column column(List<String> paths) {
        Objects.requireNonNull(paths);
        if (paths.getClass() != ArrayList.class) {
            paths = new ArrayList<>(paths);
        }
        return ExpressionTrees.newColumn(paths.toArray(new String[0]));
    }

    public static ExpressionTree operate(Expression l, Operator o, Expression r) {
        return operate(l, o, Lists.of(r));
    }

    public static ExpressionTree operate(Expression l, Operator o) {
        return operate(l, o, Lists.of());
    }

    public static ExpressionTree operate(Expression l, Operator o, List<? extends Expression> r) {
        ExpressionTree tree = l.tree();
        if (o == Operator.NOT
            && tree instanceof Operation
            && ((Operation) tree).operator() == Operator.NOT) {
            Operation operation = (Operation) tree;
            return operation.firstOperand();
        }
        if (o == Operator.NOT) {
            if (isTrue(l)) {
                return FALSE;
            } else if (isFalse(l)) {
                return TRUE;
            }
        }
        if (o == Operator.IN && r.isEmpty()) {
            log.warn("operator `in` right operands is empty");
            return FALSE;
        }
        List<ExpressionTree> operands;
        if (o.isMultivalued() && tree instanceof Operation && ((Operation) tree).operator() == o) {
            Operation lo = (Operation) tree;
            operands = new ArrayList<>(lo.operands().size() + r.size());
            operands.addAll(lo.operands());
        } else {
            operands = new ArrayList<>(r.size() + 1);
            operands.add(tree.tree());
        }
        for (Expression expression : r) {
            operands.add(expression.tree());
        }
        return ExpressionTrees.newOperation(operands, o);
    }

    public static <T> List<PathExpression<T, ?>> toExpressionList(Collection<Path<T, ?>> paths) {
        return paths.stream()
                .<PathExpression<T, ?>>map(Paths::get)
                .collect(Collectors.toList());
    }

}
