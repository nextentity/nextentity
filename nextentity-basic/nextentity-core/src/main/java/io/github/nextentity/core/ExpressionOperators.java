package io.github.nextentity.core;

import io.github.nextentity.core.api.Expression;
import io.github.nextentity.core.api.ExpressionOperator;
import io.github.nextentity.core.api.ExpressionOperator.ComparableOperator;
import io.github.nextentity.core.api.ExpressionOperator.NumberOperator;
import io.github.nextentity.core.api.ExpressionOperator.PathOperator;
import io.github.nextentity.core.api.ExpressionOperator.StringOperator;
import io.github.nextentity.core.api.Lists;
import io.github.nextentity.core.api.Path;
import io.github.nextentity.core.api.Path.ComparablePath;
import io.github.nextentity.core.api.Path.NumberPath;
import io.github.nextentity.core.api.Path.StringPath;
import io.github.nextentity.core.api.Root;
import io.github.nextentity.core.api.TypedExpression;
import io.github.nextentity.core.api.TypedExpression.BasicExpression;
import io.github.nextentity.core.api.TypedExpression.ComparableExpression;
import io.github.nextentity.core.api.TypedExpression.EntityPathExpression;
import io.github.nextentity.core.api.TypedExpression.NumberExpression;
import io.github.nextentity.core.api.TypedExpression.StringExpression;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.List;
import java.util.function.Function;

public class ExpressionOperators {

    public static <T, U extends Comparable<U>, B> ComparableOperator<T, U, B>
    ofComparable(ComparableExpression<T, U> expression, Function<? super BasicExpression<?, ?>, B> builder) {
        return new _Comparable<>(expression, builder);
    }

    public static <T, U, B> PathOperator<T, U, B> ofPath(BasicExpression<T, U> expression,
                                                         Function<? super BasicExpression<?, ?>, B> builder) {
        return new _Path<>(expression, builder);
    }

    public static <T, B> StringOperator<T, B> ofString(StringExpression<T> expression,
                                                       Function<? super BasicExpression<?, ?>, B> builder) {
        return new _String<>(expression, builder);
    }

    public static <T, U extends Number & Comparable<U>, B> NumberOperator<T, U, B>
    ofNumber(NumberExpression<T, U> expression, Function<? super BasicExpression<?, ?>, B> builder) {
        return new _Number<>(expression, builder);
    }

    static class _Basic<T, U, B> implements ExpressionOperator<T, U, B> {

        protected final BasicExpression<T, U> base;
        protected final Function<? super BasicExpression<?, ?>, B> resultBuilder;

        _Basic(BasicExpression<T, U> base, Function<? super BasicExpression<?, ?>, B> resultBuilder) {
            this.base = base;
            this.resultBuilder = resultBuilder;
        }

        @Override
        public B eq(U value) {
            return resultBuilder.apply(base.eq(value));
        }

        @Override
        public B eqIfNotNull(U value) {
            return resultBuilder.apply(value == null ? null : base.eq(value));
        }

        @Override
        public B eq(TypedExpression<T, U> expression) {
            return resultBuilder.apply(base.eq(expression));
        }

        @Override
        public B ne(U value) {
            return resultBuilder.apply(base.ne(value));
        }

        @Override
        public B neIfNotNull(U value) {
            return resultBuilder.apply(value == null ? null : base.ne(value));
        }

        @Override
        public B ne(TypedExpression<T, U> expression) {
            return resultBuilder.apply(base.ne(expression));
        }

        @Override
        @SafeVarargs
        public final B in(U... values) {
            return resultBuilder.apply(base.in(values));
        }

        @Override
        public B in(@NotNull List<? extends TypedExpression<T, U>> expressions) {
            return resultBuilder.apply(base.in(expressions));
        }

        @Override
        public B in(@NotNull TypedExpression<T, List<U>> expressions) {
            List<TypedExpression<T, U>> in = TypeCastUtil.unsafeCast(Lists.of(expressions));
            return resultBuilder.apply(base.in(in));
        }

        @Override
        public B in(@NotNull Collection<? extends U> values) {
            return resultBuilder.apply(base.in(values));
        }

        @Override
        @SafeVarargs
        public final B notIn(U... values) {
            return resultBuilder.apply(base.notIn(values));
        }

        @Override
        public B notIn(@NotNull List<? extends TypedExpression<T, U>> expressions) {
            return resultBuilder.apply(base.notIn(expressions));
        }

        @Override
        public B notIn(@NotNull Collection<? extends U> values) {
            return resultBuilder.apply(base.notIn(values));
        }

        @Override
        public B isNull() {
            return resultBuilder.apply(base.isNull());
        }

        @Override
        public B isNotNull() {
            return resultBuilder.apply(base.isNotNull());
        }


        public Expression expression() {
            return base.expression();
        }

        public Root<T> root() {
            return RootImpl.of();
        }

        public NumberExpression<T, Long> count() {
            return base.count();
        }

    }

    static class _Number<T, U extends Number & Comparable<U>, B> extends ExpressionOperators._Comparable<T, U, B> implements NumberOperator<T, U, B> {

        _Number(NumberExpression<T, U> expression, Function<? super BasicExpression<?, ?>, B> resultBuilder) {
            super(expression, resultBuilder);
        }

        @Override
        protected NumberExpression<T, U> base() {
            return (NumberExpression<T, U>) base;
        }

        @Override
        public NumberOperator<T, U, B> add(U value) {
            return new _Number<>(base().add(value), resultBuilder);
        }

        @Override
        public NumberOperator<T, U, B> subtract(U value) {
            return new _Number<>(base().subtract(value), resultBuilder);
        }

        @Override
        public NumberOperator<T, U, B> multiply(U value) {
            return new _Number<>(base().multiply(value), resultBuilder);
        }

        @Override
        public NumberOperator<T, U, B> divide(U value) {
            return new _Number<>(base().divide(value), resultBuilder);
        }

        @Override
        public NumberOperator<T, U, B> mod(U value) {
            return new _Number<>(base().mod(value), resultBuilder);
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
            return new _Number<>(base().add(expression), resultBuilder);
        }

        @Override
        public NumberOperator<T, U, B> subtract(TypedExpression<T, U> expression) {
            return new _Number<>(base().subtract(expression), resultBuilder);
        }

        @Override
        public NumberOperator<T, U, B> multiply(TypedExpression<T, U> expression) {
            return new _Number<>(base().multiply(expression), resultBuilder);
        }

        @Override
        public NumberOperator<T, U, B> divide(TypedExpression<T, U> expression) {
            return new _Number<>(base().divide(expression), resultBuilder);
        }

        @Override
        public NumberOperator<T, U, B> mod(TypedExpression<T, U> expression) {
            return new _Number<>(base().mod(expression), resultBuilder);
        }

    }

    static class _Path<T, U, B> extends _Basic<T, U, B> implements PathOperator<T, U, B> {

        _Path(BasicExpression<T, U> expression, Function<? super BasicExpression<?, ?>, B> resultBuilder) {
            super(expression, resultBuilder);
        }

        protected EntityPathExpression<T, U> base() {
            return (EntityPathExpression<T, U>) base;
        }

        @Override
        public <V> PathOperator<T, V, B> get(Path<U, V> path) {
            return ofPath(base().get(path), resultBuilder);
        }

        @Override
        public StringOperator<T, B> get(StringPath<U> path) {
            StringExpression<T> tStringExpression = base().get(path);
            return ofString(tStringExpression, resultBuilder);
        }

        @Override
        public <V extends Number & Comparable<V>> NumberOperator<T, V, B> get(NumberPath<U, V> path) {
            return new _Number<>(base().get(path), resultBuilder);
        }

        @Override
        public <V extends Comparable<V>> ComparableOperator<T, V, B> get(ComparablePath<U, V> path) {
            return ExpressionOperators.ofComparable(base().get(path), resultBuilder);
        }

    }

    static class _String<T, B> extends ExpressionOperators._Comparable<T, String, B> implements StringOperator<T, B> {

        _String(StringExpression<T> expression, Function<? super BasicExpression<?, ?>, B> resultBuilder) {
            super(expression, resultBuilder);
        }

        @Override
        protected StringExpression<T> base() {
            return (StringExpression<T>) base;
        }

        @Override
        public B like(String value) {
            return resultBuilder.apply(base().like(value));
        }

        @Override
        public B notLike(String value) {
            return resultBuilder.apply(base().notLike(value));
        }

        @Override
        public B likeIfNotNull(String value) {
            return resultBuilder.apply(value == null ? null : base().like(value));
        }

        @Override
        public B notLikeIfNotNull(String value) {
            return resultBuilder.apply(value == null ? null : base().notLike(value));
        }

        @Override
        public StringOperator<T, B> lower() {
            return ofString(base().lower(), resultBuilder);
        }

        @Override
        public StringOperator<T, B> upper() {
            return ofString(base().upper(), resultBuilder);
        }

        @Override
        public StringOperator<T, B> substring(int offset, int length) {
            return ofString(base().substring(offset, length), resultBuilder);
        }

        @Override
        public StringOperator<T, B> substring(int offset) {
            return ofString(base().substring(offset), resultBuilder);
        }

        @Override
        public StringOperator<T, B> trim() {
            return ofString(base().trim(), resultBuilder);
        }

        @Override
        public NumberOperator<T, Integer, B> length() {
            return new _Number<>(base().length(), resultBuilder);
        }
    }

    static class _Comparable<T, U extends Comparable<U>, B> extends _Basic<T, U, B> implements ComparableOperator<T, U, B> {

        _Comparable(ComparableExpression<T, U> expression, Function<? super BasicExpression<?, ?>, B> resultBuilder) {
            super(expression, resultBuilder);
        }

        protected ComparableExpression<T, U> base() {
            return (ComparableExpression<T, U>) base;
        }

        @Override
        public B ge(U value) {
            return resultBuilder.apply(base().ge(value));
        }

        @Override
        public B gt(U value) {
            return resultBuilder.apply(base().gt(value));
        }

        @Override
        public B le(U value) {
            return resultBuilder.apply(base().le(value));
        }

        @Override
        public B lt(U value) {
            return resultBuilder.apply(base().lt(value));
        }

        @Override
        public B geIfNotNull(U value) {
            return resultBuilder.apply(value == null ? null : base().ge(value));
        }

        @Override
        public B gtIfNotNull(U value) {
            return resultBuilder.apply(value == null ? null : base().gt(value));
        }

        @Override
        public B leIfNotNull(U value) {
            return resultBuilder.apply(value == null ? null : base().le(value));
        }

        @Override
        public B ltIfNotNull(U value) {
            return resultBuilder.apply(value == null ? null : base().lt(value));
        }

        @Override
        public B between(U l, U r) {
            return resultBuilder.apply(base().between(l, r));
        }

        @Override
        public B notBetween(U l, U r) {
            return resultBuilder.apply(base().notBetween(l, r));
        }

        @Override
        public B ge(TypedExpression<T, U> expression) {
            return resultBuilder.apply(base().ge(expression));
        }

        @Override
        public B gt(TypedExpression<T, U> expression) {
            return resultBuilder.apply(base().gt(expression));
        }

        @Override
        public B le(TypedExpression<T, U> expression) {
            return resultBuilder.apply(base().le(expression));
        }

        @Override
        public B lt(TypedExpression<T, U> expression) {
            return resultBuilder.apply(base().lt(expression));
        }

        @Override
        public B between(TypedExpression<T, U> l, TypedExpression<T, U> r) {
            return resultBuilder.apply(base().between(l, r));
        }

        @Override
        public B between(TypedExpression<T, U> l, U r) {
            return resultBuilder.apply(base().between(l, r));
        }

        @Override
        public B between(U l, TypedExpression<T, U> r) {
            return resultBuilder.apply(base().between(l, r));
        }

        @Override
        public B notBetween(TypedExpression<T, U> l, TypedExpression<T, U> r) {
            return resultBuilder.apply(base().notBetween(l, r));
        }

        @Override
        public B notBetween(TypedExpression<T, U> l, U r) {
            return resultBuilder.apply(base().notBetween(l, r));
        }

        @Override
        public B notBetween(U l, TypedExpression<T, U> r) {
            return resultBuilder.apply(base().notBetween(l, r));
        }
    }
}
