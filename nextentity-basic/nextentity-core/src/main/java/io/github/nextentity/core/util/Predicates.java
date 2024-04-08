package io.github.nextentity.core.util;

import io.github.nextentity.core.ExpressionTrees;
import io.github.nextentity.core.Expressions;
import io.github.nextentity.core.api.Expression;
import io.github.nextentity.core.api.Expression.Predicate;
import io.github.nextentity.core.api.ExpressionTree;

import static io.github.nextentity.core.api.Operator.NOT;

/**
 * @author HuangChengwei
 * @since 2024-03-21 14:13
 */
public interface Predicates {

    static <T> Predicate<T> of(Expression<T, Boolean> predicate) {
        return Expressions.ofBoolean(predicate);
    }

    @SafeVarargs
    static <T> Predicate<T> and(Expression<T, Boolean> predicate,
                                Expression<T, Boolean>... predicates) {
        return of(predicate).and(predicates);
    }

    @SafeVarargs
    static <T> Predicate<T> or(Expression<T, Boolean> predicate,
                               Expression<T, Boolean>... predicates) {
        return of(predicate).or(predicates);
    }

    static <T> Predicate<T> not(Expression<T, Boolean> lt) {
        ExpressionTree expression = ExpressionTrees.operate(lt.rootNode(), NOT);
        return Expressions.ofBoolean(expression);
    }

}
