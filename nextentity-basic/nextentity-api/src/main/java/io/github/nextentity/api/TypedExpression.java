package io.github.nextentity.api;

import io.github.nextentity.api.ExpressionBuilder.Conjunction;
import io.github.nextentity.api.ExpressionBuilder.Disjunction;
import io.github.nextentity.api.model.EntityRoot;
import io.github.nextentity.api.model.Order;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.List;

/**
 * expression
 *
 * @param <T> entity type
 * @param <U> expression value type
 */
@SuppressWarnings("unused")
public interface TypedExpression<T, U> extends Expression {

    interface OperatableExpression<T, U> extends TypedExpression<T, U> {

        EntityRoot<T> root();

        NumberExpression<T, Long> count();

        NumberExpression<T, Long> countDistinct();

        Predicate<T> eq(U value);

        Predicate<T> eqIfNotNull(U value);

        Predicate<T> eq(TypedExpression<T, U> value);

        Predicate<T> ne(U value);

        Predicate<T> neIfNotNull(U value);

        Predicate<T> ne(TypedExpression<T, U> value);

        Predicate<T> in(@NotNull TypedExpression<T, List<U>> expressions);

        @SuppressWarnings("unchecked")
        Predicate<T> in(U... values);

        Predicate<T> in(@NotNull List<? extends TypedExpression<T, U>> values);

        Predicate<T> in(@NotNull Collection<? extends U> values);

        @SuppressWarnings("unchecked")
        Predicate<T> notIn(U... values);

        Predicate<T> notIn(@NotNull List<? extends TypedExpression<T, U>> values);

        Predicate<T> notIn(@NotNull Collection<? extends U> values);

        Predicate<T> isNull();

        Predicate<T> isNotNull();

        Predicate<T> ge(TypedExpression<T, U> expression);

        Predicate<T> gt(TypedExpression<T, U> expression);

        Predicate<T> le(TypedExpression<T, U> expression);

        Predicate<T> lt(TypedExpression<T, U> expression);

        Predicate<T> between(TypedExpression<T, U> l, TypedExpression<T, U> r);

        Predicate<T> notBetween(TypedExpression<T, U> l, TypedExpression<T, U> r);

        default Order<T> asc() {
            return sort(SortOrder.ASC);
        }

        default Order<T> desc() {
            return sort(SortOrder.DESC);
        }

        Order<T> sort(SortOrder order);

        default Predicate<T> ge(U value) {
            return ge(root().literal(value));
        }

        default Predicate<T> gt(U value) {
            return gt(root().literal(value));
        }

        default Predicate<T> le(U value) {
            return le(root().literal(value));
        }

        default Predicate<T> lt(U value) {
            return lt(root().literal(value));
        }

        Predicate<T> geIfNotNull(U value);

        Predicate<T> gtIfNotNull(U value);

        Predicate<T> leIfNotNull(U value);

        Predicate<T> ltIfNotNull(U value);

        default Predicate<T> between(U l, U r) {
            EntityRoot<T> eb = root();
            return between(eb.literal(l), eb.literal(r));
        }

        default Predicate<T> notBetween(U l, U r) {
            EntityRoot<T> eb = root();
            return notBetween(eb.literal(l), eb.literal(r));
        }

        default Predicate<T> between(TypedExpression<T, U> l, U r) {
            return between(l, root().literal(r));
        }

        default Predicate<T> between(U l, TypedExpression<T, U> r) {
            return between(root().literal(l), r);
        }

        default Predicate<T> notBetween(TypedExpression<T, U> l, U r) {
            return notBetween(l, root().literal(r));
        }

        default Predicate<T> notBetween(U l, TypedExpression<T, U> r) {
            return notBetween(root().literal(l), r);
        }

    }

    interface BooleanPathExpression<T> extends Predicate<T>, PathExpression<T, Boolean> {
    }

    interface EntityPathExpression<T, U> extends PathExpression<T, U> {
        <R> EntityPathExpression<T, R> get(Path<U, R> path);

        StringPathExpression<T> get(Path.StringPath<U> path);

        <R extends Number> NumberPathExpression<T, R> get(Path.NumberPath<U, R> path);

        <R> PathExpression<T, R> get(PathExpression<U, R> path);

        StringPathExpression<T> get(StringPathExpression<U> path);

        BooleanPathExpression<T> get(Path.BooleanPath<T> path);

        <R extends Number> NumberPathExpression<T, R> get(NumberPathExpression<U, R> path);

    }

    interface NumberExpression<T, U extends Number> extends OperatableExpression<T, U> {
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
            return add(root().literal(value));
        }

        default NumberExpression<T, U> subtract(U value) {
            return subtract(root().literal(value));
        }

        default NumberExpression<T, U> multiply(U value) {
            return multiply(root().literal(value));
        }

        default NumberExpression<T, U> divide(U value) {
            return divide(root().literal(value));
        }

        default NumberExpression<T, U> mod(U value) {
            return mod(root().literal(value));
        }

        default NumberExpression<T, U> addIfNotNull(U value) {
            return value == null ? this : add(value);
        }

        default NumberExpression<T, U> subtractIfNotNull(U value) {
            return value == null ? this : subtract(value);
        }

        default NumberExpression<T, U> multiplyIfNotNull(U value) {
            return value == null ? this : multiply(value);
        }

        default NumberExpression<T, U> divideIfNotNull(U value) {
            return value == null ? this : divide(value);
        }

        default NumberExpression<T, U> modIfNotNull(U value) {
            return value == null ? this : mod(value);
        }

    }

    interface NumberPathExpression<T, U extends Number> extends NumberExpression<T, U>, PathExpression<T, U> {
    }

    interface PathExpression<T, U> extends OperatableExpression<T, U> {

    }

    interface Predicate<T> extends OperatableExpression<T, Boolean>, Conjunction<T>, Disjunction<T> {
        Predicate<T> not();

        Predicate<T> and(TypedExpression<T, Boolean> predicate);

        Predicate<T> or(TypedExpression<T, Boolean> predicate);

        Predicate<T> and(TypedExpression<T, Boolean>[] predicate);

        Predicate<T> or(TypedExpression<T, Boolean>[] predicate);

        Predicate<T> and(Iterable<? extends TypedExpression<T, Boolean>> predicates);

        Predicate<T> or(Iterable<? extends TypedExpression<T, Boolean>> predicates);
    }

    interface StringExpression<T> extends OperatableExpression<T, String> {
        Predicate<T> like(String value);

        default Predicate<T> startWith(String value) {
            return like(value + '%');
        }

        default Predicate<T> endsWith(String value) {
            return like('%' + value);
        }

        default Predicate<T> contains(String value) {
            return like('%' + value + '%');
        }

        Predicate<T> notLike(String value);

        default Predicate<T> notStartWith(String value) {
            return notLike(value + '%');
        }

        default Predicate<T> notEndsWith(String value) {
            return notLike('%' + value);
        }

        default Predicate<T> notContains(String value) {
            return notLike('%' + value + '%');
        }

        Predicate<T> likeIfNotNull(String value);

        default Predicate<T> startWithIfNotNull(String value) {
            return value == null ? likeIfNotNull(null) : likeIfNotNull(value + '%');
        }

        default Predicate<T> endsWithIfNotNull(String value) {
            return value == null ? likeIfNotNull(null) : likeIfNotNull('%' + value);
        }

        default Predicate<T> containsIfNotNull(String value) {
            return value == null ? likeIfNotNull(null) : likeIfNotNull('%' + value + '%');
        }

        Predicate<T> notLikeIfNotNull(String value);

        default Predicate<T> notStartWithIfNotNull(String value) {
            return value == null ? notLikeIfNotNull(null) : notLikeIfNotNull(value + '%');
        }

        default Predicate<T> notEndsWithIfNotNull(String value) {
            return value == null ? notLikeIfNotNull(null) : notLikeIfNotNull('%' + value);
        }

        default Predicate<T> notContainsIfNotNull(String value) {
            return value == null ? notLikeIfNotNull(null) : notLikeIfNotNull('%' + value + '%');
        }


        Predicate<T> likeIfNotEmpty(String value);

        default Predicate<T> startWithIfNotEmpty(String value) {
            return value == null || value.isEmpty() ? likeIfNotEmpty(null) : likeIfNotEmpty(value + '%');
        }

        default Predicate<T> endsWithIfNotEmpty(String value) {
            return value == null || value.isEmpty() ? likeIfNotEmpty(null) : likeIfNotEmpty('%' + value);
        }

        default Predicate<T> containsIfNotEmpty(String value) {
            return value == null || value.isEmpty() ? likeIfNotEmpty(null) : likeIfNotEmpty('%' + value + '%');
        }

        Predicate<T> notLikeIfNotEmpty(String value);

        default Predicate<T> notStartWithIfNotEmpty(String value) {
            return value == null || value.isEmpty() ? notLikeIfNotEmpty(null) : notLikeIfNotEmpty(value + '%');
        }

        default Predicate<T> notEndsWithIfNotEmpty(String value) {
            return value == null || value.isEmpty() ? notLikeIfNotEmpty(null) : notLikeIfNotEmpty('%' + value);
        }

        default Predicate<T> notContainsIfNotEmpty(String value) {
            return value == null || value.isEmpty() ? notLikeIfNotEmpty(null) : notLikeIfNotEmpty('%' + value + '%');
        }

        StringExpression<T> lower();

        StringExpression<T> upper();

        StringExpression<T> substring(int offset, int length);

        default StringExpression<T> substring(int offset) {
            return substring(offset, Integer.MAX_VALUE);
        }

        StringExpression<T> trim();

        NumberExpression<T, Integer> length();

        Predicate<T> eqIfNotEmpty(String value);
    }

    interface StringPathExpression<T> extends StringExpression<T>, PathExpression<T, String> {
    }
}
