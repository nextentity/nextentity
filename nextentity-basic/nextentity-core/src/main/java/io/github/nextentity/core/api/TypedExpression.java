package io.github.nextentity.core.api;

import io.github.nextentity.core.api.ExpressionOperator.AndOperator;
import io.github.nextentity.core.api.ExpressionOperator.OrOperator;
import io.github.nextentity.core.api.Order.SortOrder;
import io.github.nextentity.core.api.Path.BooleanPath;
import io.github.nextentity.core.api.Path.ComparablePath;
import io.github.nextentity.core.api.Path.NumberPath;
import io.github.nextentity.core.api.Path.StringPath;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.List;

@SuppressWarnings("unused")
public interface TypedExpression<T, U> {

    Expression expression();

    interface BasicExpression<T, U> extends TypedExpression<T, U> {

        Root<T> root();

        NumberExpression<T, Long> count();

        BooleanExpression<T> eq(U value);

        BooleanExpression<T> eqIfNotNull(U value);

        BooleanExpression<T> eq(TypedExpression<T, U> value);

        BooleanExpression<T> ne(U value);

        BooleanExpression<T> neIfNotNull(U value);

        BooleanExpression<T> ne(TypedExpression<T, U> value);

        @SuppressWarnings("unchecked")
        BooleanExpression<T> in(U... values);

        BooleanExpression<T> in(@NotNull List<? extends TypedExpression<T, U>> values);

        BooleanExpression<T> in(@NotNull Collection<? extends U> values);

        @SuppressWarnings("unchecked")
        BooleanExpression<T> notIn(U... values);

        BooleanExpression<T> notIn(@NotNull List<? extends TypedExpression<T, U>> values);

        BooleanExpression<T> notIn(@NotNull Collection<? extends U> values);

        BooleanExpression<T> isNull();

        BooleanExpression<T> isNotNull();

    }

    interface BooleanExpression<T> extends AndOperator<T>, OrOperator<T>, Predicate<T> {
    }

    interface ComparableExpression<T, U extends Comparable<U>> extends BasicExpression<T, U> {
        BooleanExpression<T> ge(TypedExpression<T, U> expression);

        BooleanExpression<T> gt(TypedExpression<T, U> expression);

        BooleanExpression<T> le(TypedExpression<T, U> expression);

        BooleanExpression<T> lt(TypedExpression<T, U> expression);

        BooleanExpression<T> between(TypedExpression<T, U> l, TypedExpression<T, U> r);

        BooleanExpression<T> notBetween(TypedExpression<T, U> l, TypedExpression<T, U> r);

        default Order<T> asc() {
            return sort(SortOrder.ASC);
        }

        default Order<T> desc() {
            return sort(SortOrder.DESC);
        }

        Order<T> sort(SortOrder order);

        default BooleanExpression<T> ge(U value) {
            return ge(root().of(value));
        }

        default BooleanExpression<T> gt(U value) {
            return gt(root().of(value));
        }

        default BooleanExpression<T> le(U value) {
            return le(root().of(value));
        }

        default BooleanExpression<T> lt(U value) {
            return lt(root().of(value));
        }

        BooleanExpression<T> geIfNotNull(U value);

        BooleanExpression<T> gtIfNotNull(U value);

        BooleanExpression<T> leIfNotNull(U value);

        BooleanExpression<T> ltIfNotNull(U value);

        default BooleanExpression<T> between(U l, U r) {
            Root<T> eb = root();
            return between(eb.of(l), eb.of(r));
        }

        default BooleanExpression<T> notBetween(U l, U r) {
            Root<T> eb = root();
            return notBetween(eb.of(l), eb.of(r));
        }

        default BooleanExpression<T> between(TypedExpression<T, U> l, U r) {
            return between(l, root().of(r));
        }

        default BooleanExpression<T> between(U l, TypedExpression<T, U> r) {
            return between(root().of(l), r);
        }

        default BooleanExpression<T> notBetween(TypedExpression<T, U> l, U r) {
            return notBetween(l, root().of(r));
        }

        default BooleanExpression<T> notBetween(U l, TypedExpression<T, U> r) {
            return notBetween(root().of(l), r);
        }

    }

    interface NumberExpression<T, U extends Number & Comparable<U>> extends ComparableExpression<T, U> {
        NumberExpression<T, U> add(TypedExpression<T, U> expression);

        NumberExpression<T, U> subtract(TypedExpression<T, U> expression);

        NumberExpression<T, U> multiply(TypedExpression<T, U> expression);

        NumberExpression<T, U> divide(TypedExpression<T, U> expression);

        NumberExpression<T, U> mod(TypedExpression<T, U> expression);

        NumberExpression<T, U> sum();

        NumberExpression<T, Double> avg();

        NumberExpression<T, U> max();

        NumberExpression<T, U> min();

        default NumberExpression<T, U> add(U value) {
            return add(root().of(value));
        }

        default NumberExpression<T, U> subtract(U value) {
            return subtract(root().of(value));
        }

        default NumberExpression<T, U> multiply(U value) {
            return multiply(root().of(value));
        }

        default NumberExpression<T, U> divide(U value) {
            return divide(root().of(value));
        }

        default NumberExpression<T, U> mod(U value) {
            return mod(root().of(value));
        }

        NumberExpression<T, U> addIfNotNull(U value);

        NumberExpression<T, U> subtractIfNotNull(U value);

        NumberExpression<T, U> multiplyIfNotNull(U value);

        NumberExpression<T, U> divideIfNotNull(U value);

        NumberExpression<T, U> modIfNotNull(U value);

    }

    interface PathExpression<T, U> extends BasicExpression<T, U> {

    }

    interface EntityPathExpression<T, U> extends PathExpression<T, U> {
        <R> EntityPathExpression<T, R> get(Path<U, R> path);

        StringPathExpression<T> get(StringPath<U> path);

        <R extends Number & Comparable<R>> NumberPathExpression<T, R> get(NumberPath<U, R> path);

        <R extends Comparable<R>> ComparablePathExpression<T, R> get(ComparablePath<U, R> path);

        BooleanPathExpression<T> get(BooleanPath<U> path);

        <R> PathExpression<T, R> get(PathExpression<U, R> path);

        StringPathExpression<T> get(StringPathExpression<U> path);

        <R extends Number & Comparable<R>> NumberPathExpression<T, R> get(NumberPathExpression<U, R> path);

        <R extends Comparable<R>> ComparablePathExpression<T, R> get(ComparablePathExpression<U, R> path);

        BooleanPathExpression<T> get(BooleanPathExpression<U> path);

    }

    interface Predicate<T> extends TypedExpression<T, Boolean> {
        Predicate<T> not();

    }

    interface StringExpression<T> extends ComparableExpression<T, String> {
        BooleanExpression<T> like(String value);

        default BooleanExpression<T> startWith(String value) {
            return like(value + '%');
        }

        default BooleanExpression<T> endsWith(String value) {
            return like('%' + value);
        }

        default BooleanExpression<T> contains(String value) {
            return like('%' + value + '%');
        }

        BooleanExpression<T> notLike(String value);

        default BooleanExpression<T> notStartWith(String value) {
            return notLike(value + '%');
        }

        default BooleanExpression<T> notEndsWith(String value) {
            return notLike('%' + value);
        }

        default BooleanExpression<T> notContains(String value) {
            return notLike('%' + value + '%');
        }

        BooleanExpression<T> likeIfNotNull(String value);

        default BooleanExpression<T> startWithIfNotNull(String value) {
            return value == null ? likeIfNotNull(null) : likeIfNotNull(value + '%');
        }

        default BooleanExpression<T> endsWithIfNotNull(String value) {
            return value == null ? likeIfNotNull(null) : likeIfNotNull('%' + value);
        }

        default BooleanExpression<T> containsIfNotNull(String value) {
            return value == null ? likeIfNotNull(null) : likeIfNotNull('%' + value + '%');
        }

        BooleanExpression<T> notLikeIfNotNull(String value);

        default BooleanExpression<T> notStartWithIfNotNull(String value) {
            return value == null ? notLikeIfNotNull(null) : notLikeIfNotNull(value + '%');
        }

        default BooleanExpression<T> notEndsWithIfNotNull(String value) {
            return value == null ? notLikeIfNotNull(null) : notLikeIfNotNull('%' + value);
        }

        default BooleanExpression<T> notContainsIfNotNull(String value) {
            return value == null ? notLikeIfNotNull(null) : notLikeIfNotNull('%' + value + '%');
        }

        StringExpression<T> lower();

        StringExpression<T> upper();

        StringExpression<T> substring(int a, int b);

        StringExpression<T> substring(int a);

        StringExpression<T> trim();

        NumberExpression<T, Integer> length();
    }

    interface BooleanPathExpression<T> extends BooleanExpression<T>, PathExpression<T, Boolean> {
    }

    interface StringPathExpression<T> extends StringExpression<T>, PathExpression<T, String> {
    }

    interface ComparablePathExpression<T, U extends Comparable<U>> extends ComparableExpression<T, U>, PathExpression<T, U> {
    }

    interface NumberPathExpression<T, U extends Number & Comparable<U>> extends NumberExpression<T, U>, PathExpression<T, U> {
    }
}
