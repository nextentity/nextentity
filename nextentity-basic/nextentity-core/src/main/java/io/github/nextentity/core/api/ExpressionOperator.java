package io.github.nextentity.core.api;

import io.github.nextentity.core.api.Path.BooleanPath;
import io.github.nextentity.core.api.Path.ComparablePath;
import io.github.nextentity.core.api.Path.NumberPath;
import io.github.nextentity.core.api.Path.StringPath;
import io.github.nextentity.core.api.Query.PredicateBuilder;
import io.github.nextentity.core.api.TypedExpression.ComparableExpression;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.List;

public interface ExpressionOperator<T, U, B> {

    B eq(U value);

    B eqIfNotNull(U value);

    B eq(TypedExpression<T, U> expression);

    B ne(U value);

    B neIfNotNull(U value);

    B ne(TypedExpression<T, U> expression);

    @SuppressWarnings({"unchecked"})
    B in(U... values);

    B in(@NotNull List<? extends TypedExpression<T, U>> expressions);

    B in(@NotNull TypedExpression<T, List<U>> expressions);

    B in(@NotNull Collection<? extends U> values);

    @SuppressWarnings({"unchecked"})
    B notIn(U... values);

    B notIn(@NotNull List<? extends TypedExpression<T, U>> expressions);

    B notIn(@NotNull Collection<? extends U> values);

    B isNull();

    B isNotNull();

    interface ComparableOperator<T, U extends Comparable<U>, B> extends ExpressionOperator<T, U, B> {

        B ge(U value);

        B gt(U value);

        B le(U value);

        B lt(U value);

        B geIfNotNull(U value);

        B gtIfNotNull(U value);

        B leIfNotNull(U value);

        B ltIfNotNull(U value);

        B between(U l, U r);

        B notBetween(U l, U r);

        B ge(TypedExpression<T, U> expression);

        B gt(TypedExpression<T, U> expression);

        B le(TypedExpression<T, U> expression);

        B lt(TypedExpression<T, U> expression);

        B between(TypedExpression<T, U> l, TypedExpression<T, U> r);

        B between(TypedExpression<T, U> l, U r);

        B between(U l, TypedExpression<T, U> r);

        B notBetween(TypedExpression<T, U> l, TypedExpression<T, U> r);

        B notBetween(TypedExpression<T, U> l, U r);

        B notBetween(U l, TypedExpression<T, U> r);

    }

    interface NumberOperator<T, U extends Number & Comparable<U>, B> extends ComparableOperator<T, U, B> {
        NumberOperator<T, U, B> add(U value);

        NumberOperator<T, U, B> subtract(U value);

        NumberOperator<T, U, B> multiply(U value);

        NumberOperator<T, U, B> divide(U value);

        NumberOperator<T, U, B> mod(U value);

        NumberOperator<T, U, B> addIfNotNull(U value);

        NumberOperator<T, U, B> subtractIfNotNull(U value);

        NumberOperator<T, U, B> multiplyIfNotNull(U value);

        NumberOperator<T, U, B> divideIfNotNull(U value);

        NumberOperator<T, U, B> modIfNotNull(U value);

        NumberOperator<T, U, B> add(TypedExpression<T, U> expression);

        NumberOperator<T, U, B> subtract(TypedExpression<T, U> expression);

        NumberOperator<T, U, B> multiply(TypedExpression<T, U> expression);

        NumberOperator<T, U, B> divide(TypedExpression<T, U> expression);

        NumberOperator<T, U, B> mod(TypedExpression<T, U> expression);

    }

    interface PathOperator<T, U, B> extends ExpressionOperator<T, U, B> {

        <V> PathOperator<T, V, B> get(Path<U, V> path);

        StringOperator<T, B> get(StringPath<U> path);

        <V extends Number & Comparable<V>> NumberOperator<T, V, B> get(NumberPath<U, V> path);

        <V extends Comparable<V>> ComparableOperator<T, V, B> get(ComparablePath<U, V> path);

        default ComparableOperator<T, Boolean, B> get(BooleanPath<U> path) {
            return get((ComparablePath<U, Boolean>) path);
        }

    }

    interface StringOperator<T, B> extends ComparableOperator<T, String, B> {

        B like(String value);

        default B startWith(String value) {
            return like(value + '%');
        }

        default B endsWith(String value) {
            return like('%' + value);
        }

        default B contains(String value) {
            return like('%' + value + '%');
        }

        B notLike(String value);

        default B notStartWith(String value) {
            return notLike(value + '%');
        }

        default B notEndsWith(String value) {
            return notLike('%' + value);
        }

        default B notContains(String value) {
            return notLike('%' + value + '%');
        }

        B likeIfNotNull(String value);

        default B startWithIfNotNull(String value) {
            return likeIfNotNull(value + '%');
        }

        default B endsWithIfNotNull(String value) {
            return likeIfNotNull('%' + value);
        }

        default B containsIfNotNull(String value) {
            return likeIfNotNull('%' + value + '%');
        }

        B notLikeIfNotNull(String value);

        default B notStartWithIfNotNull(String value) {
            return notLikeIfNotNull(value + '%');
        }

        default B notEndsWithIfNotNull(String value) {
            return notLikeIfNotNull('%' + value);
        }

        default B notContainsIfNotNull(String value) {
            return notLikeIfNotNull('%' + value + '%');
        }


        StringOperator<T, B> lower();

        StringOperator<T, B> upper();

        StringOperator<T, B> substring(int offset, int length);

        StringOperator<T, B> substring(int offset);

        StringOperator<T, B> trim();

        NumberOperator<T, Integer, B> length();

    }

    interface AndOperator<T> extends ComparableExpression<T, Boolean> {

        <R> PathOperator<T, R, ExpressionOperator.AndOperator<T>> and(Path<T, R> path);

        <R extends Comparable<R>> ComparableOperator<T, R, ExpressionOperator.AndOperator<T>> and(ComparablePath<T, R> path);

        <R extends Number & Comparable<R>> NumberOperator<T, R, ExpressionOperator.AndOperator<T>> and(NumberPath<T, R> path);

        ComparableOperator<T, Boolean, ExpressionOperator.AndOperator<T>> and(BooleanPath<T> path);

        StringOperator<T, ExpressionOperator.AndOperator<T>> and(StringPath<T> path);

        ExpressionOperator.AndOperator<T> and(TypedExpression<T, Boolean> expression);

        default ExpressionOperator.AndOperator<T> andIf(boolean predicate, PredicateBuilder<T> predicateBuilder) {
            if (predicate) {
                return and(predicateBuilder.build(root()));
            }
            return this;
        }

        ExpressionOperator.AndOperator<T> and(List<? extends TypedExpression<T, Boolean>> expressions);

        Predicate<T> then();

    }

    interface OrOperator<T> extends ComparableExpression<T, Boolean> {

        <N> PathOperator<T, N, ExpressionOperator.OrOperator<T>> or(Path<T, N> path);

        <N extends Number & Comparable<N>> NumberOperator<T, N, ExpressionOperator.OrOperator<T>> or(NumberPath<T, N> path);

        <N extends Comparable<N>> ComparableOperator<T, N, ExpressionOperator.OrOperator<T>> or(ComparablePath<T, N> path);

        StringOperator<T, ? extends ExpressionOperator.OrOperator<T>> or(StringPath<T> path);

        ExpressionOperator.ComparableOperator<T, Boolean, ExpressionOperator.OrOperator<T>> or(BooleanPath<T> path);

        ExpressionOperator.OrOperator<T> or(TypedExpression<T, Boolean> predicate);

        default ExpressionOperator.OrOperator<T> orIf(boolean predicate, PredicateBuilder<T> predicateBuilder) {
            if (predicate) {
                return or(predicateBuilder.build(root()));
            }
            return this;
        }

        ExpressionOperator.OrOperator<T> or(List<? extends TypedExpression<T, Boolean>> expressions);

        Predicate<T> then();

    }
}
