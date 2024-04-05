package io.github.nextentity.core.util;

import io.github.nextentity.core.Expressions;
import io.github.nextentity.core.TypedExpressions;
import io.github.nextentity.core.api.Expression;
import io.github.nextentity.core.api.TypedExpression;
import io.github.nextentity.core.api.TypedExpression.Predicate;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import static io.github.nextentity.core.api.Operator.AND;
import static io.github.nextentity.core.api.Operator.NOT;
import static io.github.nextentity.core.api.Operator.OR;

/**
 * @author HuangChengwei
 * @since 2024-03-21 14:13
 */
public interface Predicates {

    @SafeVarargs
    static <T> Predicate<T> and(TypedExpression<T, Boolean> predicate,
                                        TypedExpression<T, Boolean>... predicates) {
        List<Expression> metas = Arrays.stream(predicates)
                .map(TypedExpression::tree)
                .collect(Collectors.toList());
        Expression expression = Expressions.operate(predicate.tree(), AND, metas);
        return TypedExpressions.ofBoolean(expression);
    }

    @SafeVarargs
    static <T> Predicate<T> or(TypedExpression<T, Boolean> predicate,
                                       TypedExpression<T, Boolean>... predicates) {
        List<Expression> metas = Arrays.stream(predicates)
                .map(TypedExpression::tree)
                .collect(Collectors.toList());
        Expression expression = Expressions.operate(predicate.tree(), OR, metas);
        return TypedExpressions.ofBoolean(expression);
    }

    @SafeVarargs
    static <T> Predicate<T> orNot(TypedExpression<T, Boolean> predicate,
                                          TypedExpression<T, Boolean>... predicates) {
        List<Expression> metas = Arrays.stream(predicates)
                .map(TypedExpression::tree)
                .collect(Collectors.toList());
        Expression expression = Expressions.operate(predicate.tree(), OR, metas);
        expression = Expressions.operate(expression, NOT);
        return TypedExpressions.ofBoolean(expression);
    }

    static <T> Predicate<T> not(TypedExpression<T, Boolean> lt) {
        Expression expression = Expressions.operate(lt.tree(), NOT);
        return TypedExpressions.ofBoolean(expression);
    }

}
