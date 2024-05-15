package io.github.nextentity.core.expression.impl;

import io.github.nextentity.api.ExpressionBuilder;
import io.github.nextentity.api.ExpressionBuilder.NumberOperator;
import io.github.nextentity.api.ExpressionBuilder.PathOperator;
import io.github.nextentity.api.ExpressionBuilder.StringOperator;
import io.github.nextentity.api.TypedExpression.NumberExpression;
import io.github.nextentity.api.TypedExpression.OperatableExpression;
import io.github.nextentity.api.TypedExpression.StringExpression;
import io.github.nextentity.core.TypeCastUtil;
import org.jetbrains.annotations.NotNull;

import java.util.function.Function;

public class ExpressionBuilders {

    public static <T, U, B> ExpressionBuilder<T, U, B>
    of(OperatableExpression<T, U> expression, Function<? super OperatableExpression<?, ?>, B> builder) {
        return newOperator(expression, builder);
    }

    public static <T, U, B> PathOperator<T, U, B> ofPath(OperatableExpression<T, U> expression,
                                                         Function<? super OperatableExpression<?, ?>, B> builder) {
        return newOperator(expression, builder);
    }

    public static <T, B> StringOperator<T, B> ofString(StringExpression<T> expression,
                                                       Function<? super OperatableExpression<?, ?>, B> builder) {
        return newOperator(expression, builder);
    }

    public static <T, U extends Number, B> NumberOperator<T, U, B>
    ofNumber(NumberExpression<T, U> expression, Function<? super OperatableExpression<?, ?>, B> builder) {
        return newOperator(expression, builder);
    }

    @NotNull
    private static <T extends ExpressionBuilder<?, ?, ?>> T
    newOperator(OperatableExpression<?, ?> expression, Function<? super OperatableExpression<?, ?>, ?> builder) {
        return TypeCastUtil.unsafeCast(new ExpressionOperator(expression, builder));
    }

}
