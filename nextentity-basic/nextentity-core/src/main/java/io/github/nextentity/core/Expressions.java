package io.github.nextentity.core;

import io.github.nextentity.core.ExpressionTrees.ColumnImpl;
import io.github.nextentity.core.ExpressionTrees.ConstantImpl;
import io.github.nextentity.core.ExpressionTrees.OperationImpl;
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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@SuppressWarnings("PatternVariableCanBeUsed")
public interface Expressions {

    ExpressionTree TRUE = of(true);

    static boolean isNullOrTrue(Expression expression) {
        return expression == null || Expressions.isTrue(expression);
    }

    static boolean isTrue(Expression expression) {
        ExpressionTree tree = expression.tree();
        return tree instanceof Constant
               && Boolean.TRUE.equals(((Constant) tree).value());
    }

    static ExpressionTree of(Object value) {
        if (value instanceof Expression) {
            return ((Expression) value).tree();
        } else if (value instanceof Path<?, ?>) {
            return of((Path<?, ?>) value);
        }
        return new ConstantImpl<>(value);
    }

    static Column of(Path<?, ?> path) {
        String property = columnName(path);
        return column(property);
    }

    static String columnName(Path<?, ?> path) {
        return PathReference.of(path).getPropertyName();
    }

    static Column column(String path) {
        List<String> paths = new ArrayList<>(1);
        paths.add(path);
        return column(paths);
    }

    static Column column(List<String> paths) {
        Objects.requireNonNull(paths);
        if (paths.getClass() != ArrayList.class) {
            paths = new ArrayList<>(paths);
        }
        return new ColumnImpl<>(paths.toArray(String[]::new));
    }

    static ExpressionTree operate(Expression l, Operator o, Expression r) {
        return operate(l, o, Lists.of(r));
    }

    static ExpressionTree operate(Expression l, Operator o) {
        return operate(l, o, Lists.of());
    }

    static ExpressionTree operate(Expression l, Operator o, List<? extends Expression> r) {
        ExpressionTree tree = l.tree();
        if (o == Operator.NOT
            && tree instanceof Operation
            && ((Operation) tree).operator() == Operator.NOT) {
            Operation operation = (Operation) tree;
            return operation.firstOperand();
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
        return new OperationImpl<>(operands, o);
    }

    static <T> List<PathExpression<T, ?>> toExpressionList(Collection<Path<T, ?>> paths) {
        return paths.stream()
                .<PathExpression<T, ?>>map(Paths::get)
                .collect(Collectors.toList());
    }

}
