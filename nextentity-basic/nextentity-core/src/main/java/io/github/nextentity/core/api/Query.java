package io.github.nextentity.core.api;

import io.github.nextentity.core.api.Expression.PathExpression;
import io.github.nextentity.core.api.ExpressionBuilder.NumberOperator;
import io.github.nextentity.core.api.ExpressionBuilder.PathOperator;
import io.github.nextentity.core.api.ExpressionBuilder.StringOperator;
import io.github.nextentity.core.api.Path.NumberPath;
import io.github.nextentity.core.api.Path.StringPath;
import io.github.nextentity.core.api.tuple.Tuple;
import io.github.nextentity.core.api.tuple.Tuple10;
import io.github.nextentity.core.api.tuple.Tuple2;
import io.github.nextentity.core.api.tuple.Tuple3;
import io.github.nextentity.core.api.tuple.Tuple4;
import io.github.nextentity.core.api.tuple.Tuple5;
import io.github.nextentity.core.api.tuple.Tuple6;
import io.github.nextentity.core.api.tuple.Tuple7;
import io.github.nextentity.core.api.tuple.Tuple8;
import io.github.nextentity.core.api.tuple.Tuple9;
import io.github.nextentity.core.util.ImmutableList;

import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;

public interface Query {

    <T> Select<T> from(Class<T> type);

    interface Select<T> extends Fetch<T> {

        <R> Where<T, R> select(Class<R> projectionType);

        Where0<T, Tuple> select(List<? extends Expression<T, ?>> paths);

        Where0<T, Tuple> select(ExpressionsBuilder<T> selectBuilder);

        <R> Where0<T, R> select(Expression<T, R> expression);

        <R> Where0<T, R> select(Path<T, ? extends R> path);

        Where0<T, Tuple> select(Collection<Path<T, ?>> paths);

        <A, B> Where0<T, Tuple2<A, B>> select(Path<T, A> a, Path<T, B> b);

        <A, B, C> Where0<T, Tuple3<A, B, C>> select(Path<T, A> a, Path<T, B> b, Path<T, C> c);

        <A, B, C, D> Where0<T, Tuple4<A, B, C, D>> select(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d);

        <A, B, C, D, E> Where0<T, Tuple5<A, B, C, D, E>> select(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e);

        <A, B, C, D, E, F> Where0<T, Tuple6<A, B, C, D, E, F>> select(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f);

        <A, B, C, D, E, F, G> Where0<T, Tuple7<A, B, C, D, E, F, G>> select(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f, Path<T, G> g);

        <A, B, C, D, E, F, G, H> Where0<T, Tuple8<A, B, C, D, E, F, G, H>> select(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f, Path<T, G> g, Path<T, H> h);

        <A, B, C, D, E, F, G, H, I> Where0<T, Tuple9<A, B, C, D, E, F, G, H, I>> select(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f, Path<T, G> g, Path<T, H> h, Path<T, I> i);

        <A, B, C, D, E, F, G, H, I, J> Where0<T, Tuple10<A, B, C, D, E, F, G, H, I, J>> select(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f, Path<T, G> g, Path<T, H> h, Path<T, I> i, Path<T, J> j);

        <A, B> Where0<T, Tuple2<A, B>> select(Expression<T, A> a, Expression<T, B> b);

        <A, B, C> Where0<T, Tuple3<A, B, C>> select(Expression<T, A> a, Expression<T, B> b, Expression<T, C> c);

        <A, B, C, D> Where0<T, Tuple4<A, B, C, D>> select(Expression<T, A> a, Expression<T, B> b, Expression<T, C> c, Expression<T, D> d);

        <A, B, C, D, E> Where0<T, Tuple5<A, B, C, D, E>> select(Expression<T, A> a, Expression<T, B> b, Expression<T, C> c, Expression<T, D> d, Expression<T, E> e);

        <A, B, C, D, E, F> Where0<T, Tuple6<A, B, C, D, E, F>> select(Expression<T, A> a, Expression<T, B> b, Expression<T, C> c, Expression<T, D> d, Expression<T, E> e, Expression<T, F> f);

        <A, B, C, D, E, F, G> Where0<T, Tuple7<A, B, C, D, E, F, G>> select(Expression<T, A> a, Expression<T, B> b, Expression<T, C> c, Expression<T, D> d, Expression<T, E> e, Expression<T, F> f, Expression<T, G> g);

        <A, B, C, D, E, F, G, H> Where0<T, Tuple8<A, B, C, D, E, F, G, H>> select(Expression<T, A> a, Expression<T, B> b, Expression<T, C> c, Expression<T, D> d, Expression<T, E> e, Expression<T, F> f, Expression<T, G> g, Expression<T, H> h);

        <A, B, C, D, E, F, G, H, I> Where0<T, Tuple9<A, B, C, D, E, F, G, H, I>> select(Expression<T, A> a, Expression<T, B> b, Expression<T, C> c, Expression<T, D> d, Expression<T, E> e, Expression<T, F> f, Expression<T, G> g, Expression<T, H> h, Expression<T, I> i);

        <A, B, C, D, E, F, G, H, I, J> Where0<T, Tuple10<A, B, C, D, E, F, G, H, I, J>> select(Expression<T, A> a, Expression<T, B> b, Expression<T, C> c, Expression<T, D> d, Expression<T, E> e, Expression<T, F> f, Expression<T, G> g, Expression<T, H> h, Expression<T, I> i, Expression<T, J> j);

        <R> Where<T, R> selectDistinct(Class<R> projectionType);

        Where0<T, Tuple> selectDistinct(List<? extends Expression<T, ?>> paths);

        Where0<T, Tuple> selectDistinct(ExpressionsBuilder<T> selectBuilder);

        <R> Where0<T, R> selectDistinct(Expression<T, R> expression);

        <R> Where0<T, R> selectDistinct(Path<T, ? extends R> path);

        Where0<T, Tuple> selectDistinct(Collection<Path<T, ?>> paths);

        <A, B> Where0<T, Tuple2<A, B>> selectDistinct(Path<T, A> a, Path<T, B> b);

        <A, B, C> Where0<T, Tuple3<A, B, C>> selectDistinct(Path<T, A> a, Path<T, B> b, Path<T, C> c);

        <A, B, C, D> Where0<T, Tuple4<A, B, C, D>> selectDistinct(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d);

        <A, B, C, D, E> Where0<T, Tuple5<A, B, C, D, E>> selectDistinct(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e);

        <A, B, C, D, E, F> Where0<T, Tuple6<A, B, C, D, E, F>> selectDistinct(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f);

        <A, B, C, D, E, F, G> Where0<T, Tuple7<A, B, C, D, E, F, G>> selectDistinct(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f, Path<T, G> g);

        <A, B, C, D, E, F, G, H> Where0<T, Tuple8<A, B, C, D, E, F, G, H>> selectDistinct(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f, Path<T, G> g, Path<T, H> h);

        <A, B, C, D, E, F, G, H, I> Where0<T, Tuple9<A, B, C, D, E, F, G, H, I>> selectDistinct(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f, Path<T, G> g, Path<T, H> h, Path<T, I> i);

        <A, B, C, D, E, F, G, H, I, J> Where0<T, Tuple10<A, B, C, D, E, F, G, H, I, J>> selectDistinct(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f, Path<T, G> g, Path<T, H> h, Path<T, I> i, Path<T, J> j);

        <A, B> Where0<T, Tuple2<A, B>> selectDistinct(Expression<T, A> a, Expression<T, B> b);

        <A, B, C> Where0<T, Tuple3<A, B, C>> selectDistinct(Expression<T, A> a, Expression<T, B> b, Expression<T, C> c);

        <A, B, C, D> Where0<T, Tuple4<A, B, C, D>> selectDistinct(Expression<T, A> a, Expression<T, B> b, Expression<T, C> c, Expression<T, D> d);

        <A, B, C, D, E> Where0<T, Tuple5<A, B, C, D, E>> selectDistinct(Expression<T, A> a, Expression<T, B> b, Expression<T, C> c, Expression<T, D> d, Expression<T, E> e);

        <A, B, C, D, E, F> Where0<T, Tuple6<A, B, C, D, E, F>> selectDistinct(Expression<T, A> a, Expression<T, B> b, Expression<T, C> c, Expression<T, D> d, Expression<T, E> e, Expression<T, F> f);

        <A, B, C, D, E, F, G> Where0<T, Tuple7<A, B, C, D, E, F, G>> selectDistinct(Expression<T, A> a, Expression<T, B> b, Expression<T, C> c, Expression<T, D> d, Expression<T, E> e, Expression<T, F> f, Expression<T, G> g);

        <A, B, C, D, E, F, G, H> Where0<T, Tuple8<A, B, C, D, E, F, G, H>> selectDistinct(Expression<T, A> a, Expression<T, B> b, Expression<T, C> c, Expression<T, D> d, Expression<T, E> e, Expression<T, F> f, Expression<T, G> g, Expression<T, H> h);

        <A, B, C, D, E, F, G, H, I> Where0<T, Tuple9<A, B, C, D, E, F, G, H, I>> selectDistinct(Expression<T, A> a, Expression<T, B> b, Expression<T, C> c, Expression<T, D> d, Expression<T, E> e, Expression<T, F> f, Expression<T, G> g, Expression<T, H> h, Expression<T, I> i);

        <A, B, C, D, E, F, G, H, I, J> Where0<T, Tuple10<A, B, C, D, E, F, G, H, I, J>> selectDistinct(Expression<T, A> a, Expression<T, B> b, Expression<T, C> c, Expression<T, D> d, Expression<T, E> e, Expression<T, F> f, Expression<T, G> g, Expression<T, H> h, Expression<T, I> i, Expression<T, J> j);

    }

    interface Fetch<T> extends Where<T, T> {

        Where<T, T> fetch(List<PathExpression<T, ?>> expressions);

        default Where<T, T> fetch(PathExpression<T, ?> path) {
            return fetch(ImmutableList.of(path));
        }

        default Where<T, T> fetch(PathExpression<T, ?> p0, PathExpression<T, ?> p1) {
            return fetch(ImmutableList.of(p0, p1));
        }

        default Where<T, T> fetch(PathExpression<T, ?> p0, PathExpression<T, ?> p1, PathExpression<T, ?> p3) {
            return fetch(ImmutableList.of(p0, p1, p3));
        }

        default Where<T, T> fetch(Collection<Path<T, ?>> paths) {
            EntityRoot<T> root = root();
            ImmutableList<PathExpression<T, ?>> list = paths.stream()
                    .map(root::get)
                    .collect(ImmutableList.collector(paths.size()));
            return fetch(list);
        }

        default Where<T, T> fetch(Path<T, ?> path) {
            EntityRoot<T> root = root();
            return fetch(root.get(path));
        }

        default Where<T, T> fetch(Path<T, ?> p0, Path<T, ?> p1) {
            EntityRoot<T> root = root();
            return fetch(root.get(p0), root.get(p1));
        }

        default Where<T, T> fetch(Path<T, ?> p0, Path<T, ?> p1, Path<T, ?> p3) {
            EntityRoot<T> root = root();
            return fetch(root.get(p0), root.get(p1), root.get(p3));
        }

    }

    interface Where<T, U> extends OrderBy<T, U> {

        Where<T, U> where(Expression<T, Boolean> predicate);

        default Where<T, U> where(PredicateBuilder<T> predicateBuilder) {
            return where(predicateBuilder.build(root()));
        }

        default Where<T, U> whereIf(boolean predicate, PredicateBuilder<T> predicateBuilder) {
            if (predicate) {
                return where(predicateBuilder);
            } else {
                return this;
            }
        }

        <N> PathOperator<T, N, ? extends Where<T, U>> where(Path<T, N> path);

        <N extends Number> NumberOperator<T, N, ? extends Where<T, U>> where(NumberPath<T, N> path);

        StringOperator<T, ? extends Where<T, U>> where(StringPath<T> path);

    }

    interface Where0<T, U> extends GroupBy<T, U>, Where<T, U> {

        Where0<T, U> where(Expression<T, Boolean> predicate);

        default Where0<T, U> where(PredicateBuilder<T> predicateBuilder) {
            return where(predicateBuilder.build(root()));
        }

        default Where0<T, U> whereIf(boolean predicate, PredicateBuilder<T> predicateBuilder) {
            if (predicate) {
                return where(predicateBuilder.build(root()));
            }
            return this;
        }

        <N> PathOperator<T, N, Where0<T, U>> where(Path<T, N> path);

        <N extends Number> NumberOperator<T, N, Where0<T, U>> where(NumberPath<T, N> path);

        StringOperator<T, Where0<T, U>> where(StringPath<T> path);

    }

    interface GroupBy<T, U> extends OrderBy<T, U> {
        Having<T, U> groupBy(List<? extends Expression<T, ?>> expressions);

        Having<T, U> groupBy(ExpressionsBuilder<T> expressionsBuilder);

        Having<T, U> groupBy(Path<T, ?> path);

        Having<T, U> groupBy(Collection<Path<T, ?>> paths);

        default Having<T, U> groupBy(Path<T, ?> p0, Path<T, ?> p1) {
            return groupBy(ImmutableList.of(p0, p1));
        }

        default Having<T, U> groupBy(Path<T, ?> p0, Path<T, ?> p1, Path<T, ?> p2) {
            return groupBy(ImmutableList.of(p0, p1, p2));
        }

        default Having<T, U> groupBy(Path<T, ?> p0, Path<T, ?> p1, Path<T, ?> p2, Path<T, ?> p3) {
            return groupBy(ImmutableList.of(p0, p1, p2, p3));
        }

        default Having<T, U> groupBy(Path<T, ?> p0, Path<T, ?> p1, Path<T, ?> p2, Path<T, ?> p3, Path<T, ?> p4) {
            return groupBy(ImmutableList.of(p0, p1, p2, p3, p4));
        }

        default Having<T, U> groupBy(Path<T, ?> p0, Path<T, ?> p1, Path<T, ?> p2, Path<T, ?> p3, Path<T, ?> p4, Path<T, ?> p5) {
            return groupBy(ImmutableList.of(p0, p1, p2, p3, p4, p5));
        }
    }

    interface Having<T, U> extends OrderBy<T, U> {

        OrderBy<T, U> having(Expression<T, Boolean> predicate);

        default OrderBy<T, U> having(PredicateBuilder<T> predicateBuilder) {
            return having(predicateBuilder.build(root()));
        }

    }

    interface OrderBy<T, U> extends Collector<U>, EntityRootProvider<T> {

        Collector<U> orderBy(List<? extends Order<T>> orders);

        Collector<U> orderBy(Function<EntityRoot<T>, List<? extends Order<T>>> ordersBuilder);

        default Collector<U> orderBy(Order<T> order) {
            return orderBy(ImmutableList.of(order));
        }

        default Collector<U> orderBy(Order<T> p0, Order<T> p1) {
            return orderBy(ImmutableList.of(p0, p1));
        }

        default Collector<U> orderBy(Order<T> order1, Order<T> order2, Order<T> order3) {
            return orderBy(ImmutableList.of(order1, order2, order3));
        }

        OrderOperator<T, U> orderBy(Collection<Path<T, Comparable<?>>> paths);

        default OrderOperator<T, U> orderBy(Path<T, Comparable<?>> path) {
            return orderBy(ImmutableList.of(path));
        }

        default OrderOperator<T, U> orderBy(Path<T, Comparable<?>> p1, Path<T, Comparable<?>> p2) {
            return orderBy(ImmutableList.of(p1, p2));
        }

        default OrderOperator<T, U> orderBy(Path<T, Comparable<?>> p1, Path<T, Comparable<?>> p2, Path<T, Comparable<?>> p3) {
            return orderBy(ImmutableList.of(p1, p2, p3));
        }

    }

    interface EntityRootProvider<T> {
        EntityRoot<T> root();
    }

    interface OrderOperator<T, U> extends OrderBy<T, U> {
        default OrderBy<T, U> asc() {
            return sort(SortOrder.ASC);
        }

        default OrderBy<T, U> desc() {
            return sort(SortOrder.DESC);
        }

        OrderBy<T, U> sort(SortOrder order);
    }

    interface Collector<T> {

        long count();

        List<T> getList(int offset, int maxResult, LockModeType lockModeType);

        default List<T> getList(int offset, int maxResult) {
            return getList(offset, maxResult, null);
        }

        default List<T> offset(int offset) {
            return getList(offset, -1, null);
        }

        default List<T> limit(int limit) {
            return getList(0, limit, null);
        }

        boolean exist(int offset);

        default Optional<T> first() {
            return Optional.ofNullable(getFirst());
        }

        default Optional<T> first(int offset) {
            return Optional.ofNullable(getFirst(offset));
        }

        default T getFirst() {
            return getFirst(-1);
        }

        default T getFirst(int offset) {
            List<T> list = getList(offset, 1);
            return list.isEmpty() ? null : list.get(0);
        }

        default T requireSingle() {
            return Objects.requireNonNull(getSingle(-1));
        }

        default Optional<T> single() {
            return Optional.ofNullable(getSingle());
        }

        default Optional<T> single(int offset) {
            return Optional.ofNullable(getSingle(offset));
        }

        default T getSingle() {
            return getSingle(-1);
        }

        default T getSingle(int offset) {
            List<T> list = getList(offset, 2);
            if (list.size() > 1) {
                throw new IllegalStateException("found more than one");
            }
            return list.isEmpty() ? null : list.get(0);
        }

        default List<T> getList() {
            return getList(-1, -1);
        }

        default boolean exist() {
            return exist(-1);
        }

        default Optional<T> first(LockModeType lockModeType) {
            return Optional.ofNullable(getFirst(lockModeType));
        }

        default Optional<T> first(int offset, LockModeType lockModeType) {
            return Optional.ofNullable(getFirst(offset, lockModeType));
        }

        default T getFirst(LockModeType lockModeType) {
            return getFirst(-1, lockModeType);
        }

        default T getFirst(int offset, LockModeType lockModeType) {
            List<T> list = getList(offset, 1, lockModeType);
            return list.isEmpty() ? null : list.get(0);
        }

        default T requireSingle(LockModeType lockModeType) {
            return Objects.requireNonNull(getSingle(-1, lockModeType));
        }

        default Optional<T> single(LockModeType lockModeType) {
            return Optional.ofNullable(getSingle(lockModeType));
        }

        default Optional<T> single(int offset, LockModeType lockModeType) {
            return Optional.ofNullable(getSingle(offset, lockModeType));
        }

        default T getSingle(LockModeType lockModeType) {
            return getSingle(-1, lockModeType);
        }

        default T getSingle(int offset, LockModeType lockModeType) {
            List<T> list = getList(offset, 2, lockModeType);
            if (list.size() > 1) {
                throw new IllegalStateException("found more than one");
            }
            return list.isEmpty() ? null : list.get(0);
        }

        default List<T> offset(int offset, LockModeType lockModeType) {
            return getList(offset, -1, lockModeType);
        }

        default List<T> limit(int limit, LockModeType lockModeType) {
            return getList(0, limit, lockModeType);
        }

        default List<T> getList(LockModeType lockModeType) {
            return getList(-1, -1, lockModeType);
        }

        <R> R slice(Sliceable<T, R> sliceable);

        Slice<T> slice(int offset, int limit);

        <X> SubQueryBuilder<X, T> asSubQuery();

        <R> Collector<R> map(Function<? super T, ? extends R> mapper);

        Page<T> getPage(Pageable pageable);

        default <R> R getPage(PageCollector<T, R> collector) {
            return slice(collector);
        }
    }

    interface SubQueryBuilder<T, U> extends Expression<T, List<U>> {
        Expression<T, Long> count();

        Expression<T, List<U>> slice(int offset, int maxResult);

        default Expression<T, U> getSingle() {
            return getSingle(-1);
        }

        Expression<T, U> getSingle(int offset);

        default Expression<T, U> getFirst() {
            return getFirst(-1);
        }

        Expression<T, U> getFirst(int offset);
    }

    @FunctionalInterface
    interface PredicateBuilder<T> {
        Expression<T, Boolean> build(EntityRoot<T> root);
    }

    interface ExpressionsBuilder<T> {
        List<? extends Expression<T, ?>> apply(EntityRoot<T> root);
    }
}
