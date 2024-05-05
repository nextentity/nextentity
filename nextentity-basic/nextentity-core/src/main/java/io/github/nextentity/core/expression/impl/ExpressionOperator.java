package io.github.nextentity.core.expression.impl;

import io.github.nextentity.api.ExpressionBuilder;
import io.github.nextentity.api.Path;
import io.github.nextentity.api.TypedExpression;
import io.github.nextentity.core.TypeCastUtil;
import io.github.nextentity.core.expression.Expressions;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.List;
import java.util.function.Function;

@SuppressWarnings({"rawtypes", "unchecked"})
class ExpressionOperator implements ExpressionBuilder.PathOperator, ExpressionBuilder.StringOperator, ExpressionBuilder.NumberOperator {
    private final AbstractExpression base;
    private final Function<? super TypedExpression.OperatableExpression, ?> operatedCallback;

    ExpressionOperator(TypedExpression.OperatableExpression<?, ?> base, Function<? super TypedExpression.OperatableExpression<?, ?>, ?> operatedCallback) {
        this.base = ExpressionImpls.toTypedExpression(base);
        this.operatedCallback = TypeCastUtil.unsafeCast(operatedCallback);
    }

    private ExpressionOperator newOperator(TypedExpression.OperatableExpression expression) {
        return new ExpressionOperator(expression, operatedCallback);
    }

    protected Object applyCallback(TypedExpression<?, ?> expression) {
        return operatedCallback.apply(ExpressionImpls.toTypedExpression(expression));
    }

    @Override
    public Object eq(Object value) {
        return applyCallback(base.eq(value));
    }

    @Override
    public Object eqIfNotNull(Object value) {
        return applyCallback(value == null ? null : base.eq(value));
    }

    @Override
    public Object eqIfNotEmpty(String value) {
        return applyCallback(value == null || value.isEmpty() ? null : base.eq(value));
    }

    @Override
    public Object eq(TypedExpression expression) {
        return applyCallback(base.eq(expression));
    }

    @Override
    public Object ne(Object value) {
        return applyCallback(base.ne(value));
    }

    @Override
    public Object neIfNotNull(Object value) {
        return applyCallback(value == null ? null : base.ne(value));
    }

    @Override
    public Object ne(TypedExpression expression) {
        return applyCallback(base.ne(expression));
    }

    @Override
    public Object in(Object[] values) {
        return applyCallback(base.in(values));
    }

    @Override
    public Object in(@NotNull Collection values) {
        return applyCallback(base.in(values));
    }

    @Override
    public Object notIn(Object[] values) {
        return applyCallback(base.notIn(values));
    }

    @Override
    public Object notIn(@NotNull Collection notIn) {
        return applyCallback(base.notIn(notIn));
    }

    @Override
    public Object isNull() {
        return applyCallback(base.isNull());
    }

    @Override
    public Object isNotNull() {
        return applyCallback(base.isNotNull());
    }

    @Override
    public Object ge(Object value) {
        return applyCallback(base.ge(value));
    }

    @Override
    public Object gt(Object value) {
        return applyCallback(base.gt(value));
    }

    @Override
    public Object le(Object value) {
        return applyCallback(base.le(value));
    }

    @Override
    public Object lt(Object value) {
        return applyCallback(base.lt(value));
    }

    @Override
    public Object geIfNotNull(Object value) {
        return applyCallback(value == null ? null : base.ge(value));
    }

    @Override
    public Object gtIfNotNull(Object value) {
        return applyCallback(value == null ? null : base.gt(value));
    }

    @Override
    public Object leIfNotNull(Object value) {
        return applyCallback(value == null ? null : base.le(value));
    }

    @Override
    public Object ltIfNotNull(Object value) {
        return applyCallback(value == null ? null : base.lt(value));
    }

    @Override
    public Object between(Object l, Object r) {
        return applyCallback(base.between(l, r));
    }

    @Override
    public Object notBetween(Object l, Object r) {
        return applyCallback(base.notBetween(l, r));
    }

    @Override
    public Object ge(TypedExpression expression) {
        return applyCallback(base.ge(expression));
    }

    @Override
    public Object gt(TypedExpression expression) {
        return applyCallback(base.gt(expression));
    }

    @Override
    public Object le(TypedExpression expression) {
        return applyCallback(base.le(expression));
    }

    @Override
    public Object lt(TypedExpression expression) {
        return applyCallback(base.lt(expression));
    }

    @Override
    public Object between(TypedExpression l, TypedExpression r) {
        return applyCallback(base.between(l, r));
    }

    @Override
    public Object between(TypedExpression l, Object r) {
        return applyCallback(base.between(l, r));
    }

    @Override
    public Object between(Object l, TypedExpression r) {
        return applyCallback(base.between(l, r));
    }

    @Override
    public Object notBetween(TypedExpression l, TypedExpression r) {
        return applyCallback(base.notBetween(l, r));
    }

    @Override
    public Object notBetween(TypedExpression l, Object r) {
        return applyCallback(base.notBetween(l, r));
    }

    @Override
    public Object notBetween(Object l, TypedExpression r) {
        return applyCallback(base.notBetween(l, r));
    }

    @Override
    public Object notIn(@NotNull List list) {
        return applyCallback(base.notIn(list));
    }

    @Override
    public Object in(@NotNull TypedExpression expressions) {
        return applyCallback(base.in(expressions));
    }

    @Override
    public Object in(@NotNull List list) {
        return applyCallback(base.in(list));
    }

    @Override
    public NumberOperator add(Number value) {
        TypedExpression.OperatableExpression expression = base.add(value);
        return newOperator(expression);
    }

    @Override
    public NumberOperator subtract(Number value) {
        return newOperator(base.subtract(value));
    }

    @Override
    public NumberOperator multiply(Number value) {
        return newOperator(base.multiply(value));
    }

    @Override
    public NumberOperator divide(Number value) {
        return newOperator(base.divide(value));
    }

    @Override
    public NumberOperator mod(Number value) {
        return newOperator(base.mod(value));
    }

    @Override
    public NumberOperator add(TypedExpression expression) {
        return newOperator(base.add(expression));
    }

    @Override
    public NumberOperator subtract(TypedExpression expression) {
        return newOperator(base.subtract(expression));
    }

    @Override
    public NumberOperator multiply(TypedExpression expression) {
        return newOperator(base.multiply(expression));
    }

    @Override
    public NumberOperator divide(TypedExpression expression) {
        return newOperator(base.divide(expression));
    }

    @Override
    public NumberOperator mod(TypedExpression expression) {
        return newOperator(base.mod(expression));
    }

    @Override
    public PathOperator get(Path path) {
        return newOperator(base.get(path));
    }

    @Override
    public StringOperator get(Path.StringPath path) {
        return newOperator(base.get(path));
    }

    @Override
    public NumberOperator get(Path.NumberPath path) {
        return newOperator(base.get(path));
    }

    @Override
    public Object like(String value) {
        return applyCallback(base.like(value));
    }

    @Override
    public Object notLike(String value) {
        return applyCallback(base.notLike(value));
    }

    @Override
    public Object likeIfNotNull(String value) {
        return applyCallback(value == null ? null : base.like(value));
    }

    @Override
    public Object notLikeIfNotNull(String value) {
        return applyCallback(value == null ? null : base.notLike(value));
    }

    @Override
    public Object notLikeIfNotEmpty(String value) {
        return applyCallback(value == null || value.isEmpty() ? null : base.notLike(value));
    }

    @Override
    public StringOperator lower() {
        return newOperator(base.lower());
    }

    @Override
    public StringOperator upper() {
        return newOperator(base.upper());
    }

    @Override
    public StringOperator substring(int offset, int length) {
        return newOperator(base.substring(offset, length));
    }

    @Override
    public StringOperator trim() {
        return newOperator(base.trim());
    }

    @Override
    public NumberOperator length() {
        return newOperator(base.length());
    }
}
