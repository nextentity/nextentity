package io.github.nextentity.core;

import io.github.nextentity.core.api.Expression;
import io.github.nextentity.core.api.ExpressionOperator;
import io.github.nextentity.core.api.ExpressionOperator.NumberOperator;
import io.github.nextentity.core.api.ExpressionOperator.PathOperator;
import io.github.nextentity.core.api.ExpressionOperator.StringOperator;
import io.github.nextentity.core.api.Lists;
import io.github.nextentity.core.api.Path;
import io.github.nextentity.core.api.Path.NumberPath;
import io.github.nextentity.core.api.Path.StringPath;
import io.github.nextentity.core.api.Root;
import io.github.nextentity.core.api.TypedExpression;
import io.github.nextentity.core.api.TypedExpression.BasicExpression;
import io.github.nextentity.core.api.TypedExpression.EntityPathExpression;
import io.github.nextentity.core.api.TypedExpression.NumberExpression;
import io.github.nextentity.core.api.TypedExpression.StringExpression;
import io.github.nextentity.core.util.Paths;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.List;
import java.util.function.Function;

public class ExpressionOperators {

    public static <T, U, B> ExpressionOperator<T, U, B>
    of(BasicExpression<T, U> expression, Function<? super BasicExpression<?, ?>, B> builder) {
        return new _Expression<>(expression, builder);
    }

    public static <T, U, B> PathOperator<T, U, B> ofPath(BasicExpression<T, U> expression,
                                                         Function<? super BasicExpression<?, ?>, B> builder) {
        return new _Path<>(expression, builder);
    }

    public static <T, B> StringOperator<T, B> ofString(StringExpression<T> expression,
                                                       Function<? super BasicExpression<?, ?>, B> builder) {
        return new _String<>(expression, builder);
    }

    public static <T, U extends Number, B> NumberOperator<T, U, B>
    ofNumber(NumberExpression<T, U> expression, Function<? super BasicExpression<?, ?>, B> builder) {
        return new _Number<>(expression, builder);
    }

    static class _Expression<T, U, B> implements ExpressionOperator<T, U, B> {

        protected final BasicExpression<T, U> base;
        protected final Function<? super BasicExpression<?, ?>, B> operatedCallback;

        _Expression(BasicExpression<T, U> base, Function<? super BasicExpression<?, ?>, B> operatedCallback) {
            this.base = base;
            this.operatedCallback = operatedCallback;
        }

        @Override
        public B eq(U value) {
            return applyCallback(base.eq(value));
        }

        @Override
        public B eqIfNotNull(U value) {
            return applyCallback(value == null ? null : base.eq(value));
        }

        @Override
        public B eq(TypedExpression<T, U> expression) {
            return applyCallback(base.eq(expression));
        }

        @Override
        public B ne(U value) {
            return applyCallback(base.ne(value));
        }

        @Override
        public B neIfNotNull(U value) {
            return applyCallback(value == null ? null : base.ne(value));
        }

        @Override
        public B ne(TypedExpression<T, U> expression) {
            return applyCallback(base.ne(expression));
        }

        @Override
        @SafeVarargs
        public final B in(U... values) {
            return applyCallback(base.in(values));
        }

        @Override
        public B in(@NotNull List<? extends TypedExpression<T, U>> expressions) {
            return applyCallback(base.in(expressions));
        }

        @Override
        public B in(@NotNull TypedExpression<T, List<U>> expressions) {
            List<TypedExpression<T, U>> in = TypeCastUtil.unsafeCast(Lists.of(expressions));
            return applyCallback(base.in(in));
        }

        @Override
        public B in(@NotNull Collection<? extends U> values) {
            return applyCallback(base.in(values));
        }

        @Override
        @SafeVarargs
        public final B notIn(U... values) {
            return applyCallback(base.notIn(values));
        }

        @Override
        public B notIn(@NotNull List<? extends TypedExpression<T, U>> expressions) {
            return applyCallback(base.notIn(expressions));
        }

        @Override
        public B notIn(@NotNull Collection<? extends U> values) {
            return applyCallback(base.notIn(values));
        }

        @Override
        public B isNull() {
            return applyCallback(base.isNull());
        }

        @Override
        public B isNotNull() {
            return applyCallback(base.isNotNull());
        }


        public Expression expression() {
            return base.expression();
        }

        public Root<T> root() {
            return Paths.root();
        }

        public NumberExpression<T, Long> count() {
            return base.count();
        }

        protected BasicExpression<T, U> base() {
            return base;
        }

        @Override
        public B ge(U value) {
            return applyCallback(base().ge(value));
        }

        @Override
        public B gt(U value) {
            return applyCallback(base().gt(value));
        }

        @Override
        public B le(U value) {
            return applyCallback(base().le(value));
        }

        @Override
        public B lt(U value) {
            return applyCallback(base().lt(value));
        }

        @Override
        public B geIfNotNull(U value) {
            return applyCallback(value == null ? null : base().ge(value));
        }

        @Override
        public B gtIfNotNull(U value) {
            return applyCallback(value == null ? null : base().gt(value));
        }

        @Override
        public B leIfNotNull(U value) {
            return applyCallback(value == null ? null : base().le(value));
        }

        @Override
        public B ltIfNotNull(U value) {
            return applyCallback(value == null ? null : base().lt(value));
        }

        @Override
        public B between(U l, U r) {
            return applyCallback(base().between(l, r));
        }

        @Override
        public B notBetween(U l, U r) {
            return applyCallback(base().notBetween(l, r));
        }

        @Override
        public B ge(TypedExpression<T, U> expression) {
            return applyCallback(base().ge(expression));
        }

        @Override
        public B gt(TypedExpression<T, U> expression) {
            return applyCallback(base().gt(expression));
        }

        @Override
        public B le(TypedExpression<T, U> expression) {
            return applyCallback(base().le(expression));
        }

        @Override
        public B lt(TypedExpression<T, U> expression) {
            return applyCallback(base().lt(expression));
        }

        @Override
        public B between(TypedExpression<T, U> l, TypedExpression<T, U> r) {
            return applyCallback(base().between(l, r));
        }

        @Override
        public B between(TypedExpression<T, U> l, U r) {
            return applyCallback(base().between(l, r));
        }

        @Override
        public B between(U l, TypedExpression<T, U> r) {
            return applyCallback(base().between(l, r));
        }

        @Override
        public B notBetween(TypedExpression<T, U> l, TypedExpression<T, U> r) {
            return applyCallback(base().notBetween(l, r));
        }

        @Override
        public B notBetween(TypedExpression<T, U> l, U r) {
            return applyCallback(base().notBetween(l, r));
        }

        @Override
        public B notBetween(U l, TypedExpression<T, U> r) {
            return applyCallback(base().notBetween(l, r));
        }

        protected B applyCallback(PredicateOperator<?> expression) {
            BasicExpression<?, ?> basic = expression instanceof BasicExpression<?, ?> || expression == null
                    ? (BasicExpression<?, ?>) expression
                    : TypedExpressions.ofBasic(expression.expression());
            return operatedCallback.apply(basic);
        }

    }

    static class _Number<T, U extends Number, B> extends _Expression<T, U, B> implements NumberOperator<T, U, B> {

        _Number(NumberExpression<T, U> expression, Function<? super BasicExpression<?, ?>, B> operatedCallback) {
            super(expression, operatedCallback);
        }

        @Override
        protected NumberExpression<T, U> base() {
            return (NumberExpression<T, U>) base;
        }

        @Override
        public NumberOperator<T, U, B> add(U value) {
            return new _Number<>(base().add(value), operatedCallback);
        }

        @Override
        public NumberOperator<T, U, B> subtract(U value) {
            return new _Number<>(base().subtract(value), operatedCallback);
        }

        @Override
        public NumberOperator<T, U, B> multiply(U value) {
            return new _Number<>(base().multiply(value), operatedCallback);
        }

        @Override
        public NumberOperator<T, U, B> divide(U value) {
            return new _Number<>(base().divide(value), operatedCallback);
        }

        @Override
        public NumberOperator<T, U, B> mod(U value) {
            return new _Number<>(base().mod(value), operatedCallback);
        }

        @Override
        public NumberOperator<T, U, B> addIfNotNull(U value) {
            return value == null ? this : add(value);
        }

        @Override
        public NumberOperator<T, U, B> subtractIfNotNull(U value) {
            return value == null ? this : subtract(value);
        }

        @Override
        public NumberOperator<T, U, B> multiplyIfNotNull(U value) {
            return value == null ? this : multiply(value);
        }

        @Override
        public NumberOperator<T, U, B> divideIfNotNull(U value) {
            return value == null ? this : divide(value);
        }

        @Override
        public NumberOperator<T, U, B> modIfNotNull(U value) {
            return value == null ? this : mod(value);
        }

        @Override
        public NumberOperator<T, U, B> add(TypedExpression<T, U> expression) {
            return new _Number<>(base().add(expression), operatedCallback);
        }

        @Override
        public NumberOperator<T, U, B> subtract(TypedExpression<T, U> expression) {
            return new _Number<>(base().subtract(expression), operatedCallback);
        }

        @Override
        public NumberOperator<T, U, B> multiply(TypedExpression<T, U> expression) {
            return new _Number<>(base().multiply(expression), operatedCallback);
        }

        @Override
        public NumberOperator<T, U, B> divide(TypedExpression<T, U> expression) {
            return new _Number<>(base().divide(expression), operatedCallback);
        }

        @Override
        public NumberOperator<T, U, B> mod(TypedExpression<T, U> expression) {
            return new _Number<>(base().mod(expression), operatedCallback);
        }

    }

    static class _Path<T, U, B> extends _Expression<T, U, B> implements PathOperator<T, U, B> {

        _Path(BasicExpression<T, U> expression, Function<? super BasicExpression<?, ?>, B> operatedCallback) {
            super(expression, operatedCallback);
        }

        protected EntityPathExpression<T, U> base() {
            return (EntityPathExpression<T, U>) base;
        }

        @Override
        public <V> PathOperator<T, V, B> get(Path<U, V> path) {
            return ofPath(base().get(path), operatedCallback);
        }

        @Override
        public StringOperator<T, B> get(StringPath<U> path) {
            StringExpression<T> tStringExpression = base().get(path);
            return ofString(tStringExpression, operatedCallback);
        }

        @Override
        public <V extends Number> NumberOperator<T, V, B> get(NumberPath<U, V> path) {
            return new _Number<>(base().get(path), operatedCallback);
        }

    }

    static class _String<T, B> extends _Expression<T, String, B> implements StringOperator<T, B> {

        _String(StringExpression<T> expression, Function<? super BasicExpression<?, ?>, B> operatedCallback) {
            super(expression, operatedCallback);
        }

        @Override
        protected StringExpression<T> base() {
            return (StringExpression<T>) base;
        }

        @Override
        public B like(String value) {
            return applyCallback(base().like(value));
        }

        @Override
        public B notLike(String value) {
            return applyCallback(base().notLike(value));
        }

        @Override
        public B likeIfNotNull(String value) {
            return applyCallback(value == null ? null : base().like(value));
        }

        @Override
        public B notLikeIfNotNull(String value) {
            return applyCallback(value == null ? null : base().notLike(value));
        }

        @Override
        public StringOperator<T, B> lower() {
            return ofString(base().lower(), operatedCallback);
        }

        @Override
        public StringOperator<T, B> upper() {
            return ofString(base().upper(), operatedCallback);
        }

        @Override
        public StringOperator<T, B> substring(int offset, int length) {
            return ofString(base().substring(offset, length), operatedCallback);
        }

        @Override
        public StringOperator<T, B> substring(int offset) {
            return ofString(base().substring(offset), operatedCallback);
        }

        @Override
        public StringOperator<T, B> trim() {
            return ofString(base().trim(), operatedCallback);
        }

        @Override
        public NumberOperator<T, Integer, B> length() {
            return new _Number<>(base().length(), operatedCallback);
        }
    }
}
