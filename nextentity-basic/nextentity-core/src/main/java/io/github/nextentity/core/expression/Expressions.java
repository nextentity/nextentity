package io.github.nextentity.core.expression;

import io.github.nextentity.api.Expression;
import io.github.nextentity.api.TypedExpression;
import io.github.nextentity.api.TypedExpression.BooleanPathExpression;
import io.github.nextentity.api.TypedExpression.EntityPathExpression;
import io.github.nextentity.api.TypedExpression.NumberExpression;
import io.github.nextentity.api.TypedExpression.NumberPathExpression;
import io.github.nextentity.api.TypedExpression.OperatableExpression;
import io.github.nextentity.api.TypedExpression.PathExpression;
import io.github.nextentity.api.TypedExpression.Predicate;
import io.github.nextentity.api.TypedExpression.StringExpression;
import io.github.nextentity.api.TypedExpression.StringPathExpression;
import io.github.nextentity.core.TypeCastUtil;
import io.github.nextentity.core.expression.impl.ExpressionImpls;

public class Expressions {
    public static <T, U> OperatableExpression<T, U> of(Expression expression) {
        return toTypedExpression(expression);
    }

    public static <T, U> TypedExpression<T, U> of(U value) {
        return of(ExpressionImpls.literal(value));
    }

    public static <T> Predicate<T> ofTrue() {
        return toTypedExpression(ExpressionImpls.TRUE);
    }

    public static <T> Predicate<T> ofFalse() {
        return toTypedExpression(ExpressionImpls.FALSE);
    }

    public static <T, R> PathExpression<T, R> ofPath(EntityPath column) {
        return toTypedExpression(column);
    }

    public static <T, R> EntityPathExpression<T, R> ofEntity(EntityPath column) {
        return toTypedExpression(column);
    }

    public static <T> StringPathExpression<T> ofString(EntityPath column) {
        return toTypedExpression(column);
    }

    public static <T, U extends Number> NumberPathExpression<T, U> ofNumber(EntityPath column) {
        return toTypedExpression(column);
    }

    public static <T, R> OperatableExpression<T, R> ofBasic(Expression expression) {
        return toTypedExpression(expression);
    }

    public static <T> StringExpression<T> ofString(Expression expression) {
        return toTypedExpression(expression);
    }

    public static <T> Predicate<T> ofBoolean(Expression expression) {
        return toTypedExpression(expression);
    }

    public static <T> BooleanPathExpression<T> ofBoolean(EntityPath expression) {
        return toTypedExpression(expression);
    }

    public static <T, U extends Number> NumberExpression<T, U> ofNumber(Expression expression) {
        return toTypedExpression(expression);
    }

    public static <T> Predicate<T> ofPredicate(Expression expression) {
        return toTypedExpression(expression);
    }

    static <T extends TypedExpression<?, ?>> T toTypedExpression(Expression expression) {
        return TypeCastUtil.unsafeCast(expression);
    }

}
