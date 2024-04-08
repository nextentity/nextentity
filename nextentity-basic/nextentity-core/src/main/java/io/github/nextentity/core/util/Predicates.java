package io.github.nextentity.core.util;

import io.github.nextentity.core.Expressions;
import io.github.nextentity.core.TypedExpressions;
import io.github.nextentity.core.api.Expression;
import io.github.nextentity.core.api.TypedExpression;
import io.github.nextentity.core.api.TypedExpression.Predicate;

import static io.github.nextentity.core.api.Operator.NOT;

/**
 * @author HuangChengwei
 * @since 2024-03-21 14:13
 */
public interface Predicates {

    static <T> Predicate<T> of(TypedExpression<T, Boolean> predicate) {
        return TypedExpressions.ofBoolean(predicate);
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

    @SafeVarargs
    static <T> Predicate<T> orNot(TypedExpression<T, Boolean> predicate,
                                  TypedExpression<T, Boolean>... predicates) {
        return of(predicate).or(predicates).not();
    }

    static <T> Predicate<T> not(TypedExpression<T, Boolean> lt) {
        Expression expression = Expressions.operate(lt.tree(), NOT);
        return TypedExpressions.ofBoolean(expression);
    }

}
