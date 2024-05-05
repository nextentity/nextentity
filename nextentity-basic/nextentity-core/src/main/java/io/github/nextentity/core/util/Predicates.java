package io.github.nextentity.core.util;

import io.github.nextentity.api.Expression;
import io.github.nextentity.api.TypedExpression;
import io.github.nextentity.api.TypedExpression.Predicate;
import io.github.nextentity.core.expression.impl.ExpressionImpls;
import io.github.nextentity.core.expression.Expressions;

import static io.github.nextentity.core.expression.Operator.NOT;

/**
 * @author HuangChengwei
 * @since 2024-03-21 14:13
 */
public interface Predicates {

    static <T> Predicate<T> of(TypedExpression<T, Boolean> predicate) {
        return Expressions.ofBoolean(predicate);
    }

    @SafeVarargs
    static <T> Predicate<T> and(TypedExpression<T, Boolean> predicate,
                                TypedExpression<T, Boolean>... predicates) {
        return of(predicate).and(predicates);
    }

    @SafeVarargs
    static <T> Predicate<T> or(TypedExpression<T, Boolean> predicate,
                               TypedExpression<T, Boolean>... predicates) {
        return of(predicate).or(predicates);
    }

    static <T> Predicate<T> not(TypedExpression<T, Boolean> lt) {
        Expression expression = ExpressionImpls.operate(lt, NOT);
        return Expressions.ofBoolean(expression);
    }

}
