package io.github.nextentity.core.api;

import io.github.nextentity.core.api.Path.NumberPath;
import io.github.nextentity.core.api.Path.StringPath;
import io.github.nextentity.core.api.Query.PredicateBuilder;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.List;

public interface ExpressionBuilder<T, U, B> {

    B eq(U value);

    B eqIfNotNull(U value);

    B eq(Expression<T, U> expression);

    B ne(U value);

    B neIfNotNull(U value);

    B ne(Expression<T, U> expression);

    @SuppressWarnings({"unchecked"})
    B in(U... values);

    B in(@NotNull List<? extends Expression<T, U>> expressions);

    B in(@NotNull Expression<T, List<U>> expressions);

    B in(@NotNull Collection<? extends U> values);

    @SuppressWarnings({"unchecked"})
    B notIn(U... values);

    B notIn(@NotNull List<? extends Expression<T, U>> expressions);

    B notIn(@NotNull Collection<? extends U> values);

    B isNull();

    B isNotNull();

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

    B ge(Expression<T, U> expression);

    B gt(Expression<T, U> expression);

    B le(Expression<T, U> expression);

    B lt(Expression<T, U> expression);

    B between(Expression<T, U> l, Expression<T, U> r);

    B between(Expression<T, U> l, U r);

    B between(U l, Expression<T, U> r);

    B notBetween(Expression<T, U> l, Expression<T, U> r);

    B notBetween(Expression<T, U> l, U r);

    B notBetween(U l, Expression<T, U> r);

    interface NumberOperator<T, U extends Number, B> extends ExpressionBuilder<T, U, B> {
        NumberOperator<T, U, B> add(U value);

        NumberOperator<T, U, B> subtract(U value);

        NumberOperator<T, U, B> multiply(U value);

        NumberOperator<T, U, B> divide(U value);

        NumberOperator<T, U, B> mod(U value);

        default NumberOperator<T, U, B> addIfNotNull(U value) {
            return value == null ? this : add(value);
        }

        default NumberOperator<T, U, B> subtractIfNotNull(U value) {
            return value == null ? this : subtract(value);
        }

        default NumberOperator<T, U, B> multiplyIfNotNull(U value) {
            return value == null ? this : multiply(value);
        }

        default NumberOperator<T, U, B> divideIfNotNull(U value) {
            return value == null ? this : divide(value);
        }

        default NumberOperator<T, U, B> modIfNotNull(U value) {
            return value == null ? this : mod(value);
        }

        NumberOperator<T, U, B> add(Expression<T, U> expression);

        NumberOperator<T, U, B> subtract(Expression<T, U> expression);

        NumberOperator<T, U, B> multiply(Expression<T, U> expression);

        NumberOperator<T, U, B> divide(Expression<T, U> expression);

        NumberOperator<T, U, B> mod(Expression<T, U> expression);

    }

    interface PathOperator<T, U, B> extends ExpressionBuilder<T, U, B> {

        <V> PathOperator<T, V, B> get(Path<U, V> path);

        StringOperator<T, B> get(StringPath<U> path);

        <V extends Number> NumberOperator<T, V, B> get(NumberPath<U, V> path);

    }

    // TODO 未添加测试用例
    interface StringOperator<T, B> extends ExpressionBuilder<T, String, B> {

        B eqIfNotEmpty(String value);

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
            return likeIfNotNull(value == null ? null : value + '%');
        }

        default B endsWithIfNotNull(String value) {
            return likeIfNotNull(value == null ? null : '%' + value);
        }

        default B containsIfNotNull(String value) {
            return likeIfNotNull(value == null ? null : '%' + value + '%');
        }

        B notLikeIfNotNull(String value);

        default B notStartWithIfNotNull(String value) {
            return notLikeIfNotNull(value == null ? null : value + '%');
        }

        default B notEndsWithIfNotNull(String value) {
            return notLikeIfNotNull(value == null ? null : '%' + value);
        }

        default B notContainsIfNotNull(String value) {
            return notLikeIfNotNull(value == null ? null : '%' + value + '%');
        }

        default B likeIfNotEmpty(String value) {
            return value == null || value.isEmpty() ? likeIfNotNull(null) : like(value);
        }

        default B startWithIfNotEmpty(String value) {
            return likeIfNotEmpty(value == null || value.isEmpty() ? null : value + '%');
        }

        default B endsWithIfNotEmpty(String value) {
            return likeIfNotEmpty(value == null || value.isEmpty() ? null : '%' + value);
        }

        default B containsIfNotEmpty(String value) {
            return likeIfNotEmpty(value == null || value.isEmpty() ? null : '%' + value + '%');
        }

        B notLikeIfNotEmpty(String value);

        default B notStartWithIfNotEmpty(String value) {
            return notLikeIfNotEmpty(value == null || value.isEmpty() ? null : value + '%');
        }

        default B notEndsWithIfNotEmpty(String value) {
            return notLikeIfNotEmpty(value == null || value.isEmpty() ? null : '%' + value);
        }

        default B notContainsIfNotEmpty(String value) {
            return notLikeIfNotNull(value == null || value.isEmpty() ? null : '%' + value + '%');
        }

        StringOperator<T, B> lower();

        StringOperator<T, B> upper();

        StringOperator<T, B> substring(int offset, int length);

        default StringOperator<T, B> substring(int offset) {
            return substring(offset, Integer.MAX_VALUE);
        }

        StringOperator<T, B> trim();

        NumberOperator<T, Integer, B> length();

    }

    interface Conjunction<T> extends Expression<T, Boolean> {

        <R> PathOperator<T, R, Conjunction<T>> and(Path<T, R> path);

        <R extends Number> NumberOperator<T, R, Conjunction<T>> and(NumberPath<T, R> path);

        StringOperator<T, Conjunction<T>> and(StringPath<T> path);

        Conjunction<T> and(Expression<T, Boolean> expression);

        Conjunction<T> andIf(boolean predicate, PredicateBuilder<T> predicateBuilder);

        Conjunction<T> and(Iterable<? extends Expression<T, Boolean>> expressions);

        Predicate<T> toPredicate();

    }

    interface Disjunction<T> extends Expression<T, Boolean> {

        <N> PathOperator<T, N, Disjunction<T>> or(Path<T, N> path);

        <N extends Number> NumberOperator<T, N, Disjunction<T>> or(NumberPath<T, N> path);

        StringOperator<T, ? extends Disjunction<T>> or(StringPath<T> path);

        Disjunction<T> or(Expression<T, Boolean> predicate);

        Disjunction<T> orIf(boolean predicate, PredicateBuilder<T> predicateBuilder);

        Disjunction<T> or(Iterable<? extends Expression<T, Boolean>> expressions);

        Predicate<T> toPredicate();

    }
}
