package io.github.nextentity.core;

import io.github.nextentity.core.api.Entities;
import io.github.nextentity.core.api.EntityRoot;
import io.github.nextentity.core.api.Expression;
import io.github.nextentity.core.api.Expression.BooleanPathExpression;
import io.github.nextentity.core.api.Expression.EntityPathExpression;
import io.github.nextentity.core.api.Expression.NumberPathExpression;
import io.github.nextentity.core.api.Expression.PathExpression;
import io.github.nextentity.core.api.Expression.StringPathExpression;
import io.github.nextentity.core.api.ExpressionBuilder.NumberOperator;
import io.github.nextentity.core.api.ExpressionBuilder.PathOperator;
import io.github.nextentity.core.api.ExpressionBuilder.StringOperator;
import io.github.nextentity.core.api.ExpressionTree.QueryStructure.Order;
import io.github.nextentity.core.api.LockModeType;
import io.github.nextentity.core.api.Page;
import io.github.nextentity.core.api.Pageable;
import io.github.nextentity.core.api.Path;
import io.github.nextentity.core.api.Path.BooleanPath;
import io.github.nextentity.core.api.Path.NumberPath;
import io.github.nextentity.core.api.Path.StringPath;
import io.github.nextentity.core.api.Query.Collector;
import io.github.nextentity.core.api.Query.ExpressionsBuilder;
import io.github.nextentity.core.api.Query.OrderOperator;
import io.github.nextentity.core.api.Query.PredicateBuilder;
import io.github.nextentity.core.api.Query.QueryStructureBuilder;
import io.github.nextentity.core.api.Query.SubQueryBuilder;
import io.github.nextentity.core.api.Query.Where;
import io.github.nextentity.core.api.Query.Where0;
import io.github.nextentity.core.api.Slice;
import io.github.nextentity.core.api.Sliceable;
import io.github.nextentity.core.util.tuple.Tuple;
import io.github.nextentity.core.util.tuple.Tuple10;
import io.github.nextentity.core.util.tuple.Tuple2;
import io.github.nextentity.core.util.tuple.Tuple3;
import io.github.nextentity.core.util.tuple.Tuple4;
import io.github.nextentity.core.util.tuple.Tuple5;
import io.github.nextentity.core.util.tuple.Tuple6;
import io.github.nextentity.core.util.tuple.Tuple7;
import io.github.nextentity.core.util.tuple.Tuple8;
import io.github.nextentity.core.util.tuple.Tuple9;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;

/**
 * @author HuangChengwei
 * @since 2024-04-08 15:08
 */
public class EntitiesFaced<ID, T> implements Entities<ID, T> {

    private final Entities<ID, T> target;

    public EntitiesFaced(Entities<ID, T> target) {
        this.target = target;
    }

    // @formatter:off mechanically generated
























    @Override
    public T get(ID id) {
        return target.get(id);
    }

    @Override
    public List<T> getAll(Iterable<? extends ID> ids) {
        return target.getAll(ids);
    }

    @Override
    public Map<ID, T> getMap(Iterable<? extends ID> ids) {
        return target.getMap(ids);
    }

    @Override
    public <U> Expression<T, U> literal(U value) {
        return target.literal(value);
    }

    @Override
    public <U> EntityPathExpression<T, U> get(Path<T, U> path) {
        return target.get(path);
    }

    @Override
    public BooleanPathExpression<T> get(BooleanPath<T> path) {
        return target.get(path);
    }

    @Override
    public StringPathExpression<T> get(StringPath<T> path) {
        return target.get(path);
    }

    @Override
    public <U extends Number> NumberPathExpression<T, U> get(NumberPath<T, U> path) {
        return target.get(path);
    }

    @Override
    public <U> PathExpression<T, U> path(Path<T, U> path) {
        return target.path(path);
    }

    @Override
    public <U> EntityPathExpression<T, U> entity(Path<T, U> path) {
        return target.entity(path);
    }

    @Override
    public StringPathExpression<T> string(Path<T, String> path) {
        return target.string(path);
    }

    @Override
    public <U extends Number> NumberPathExpression<T, U> number(Path<T, U> path) {
        return target.number(path);
    }

    @Override
    public BooleanPathExpression<T> bool(Path<T, Boolean> path) {
        return target.bool(path);
    }

    @Override
    public <R> Where<T, R> select(Class<R> projectionType) {
        return target.select(projectionType);
    }

    @Override
    public Where0<T, Tuple> select(List<? extends Expression<T, ?>> paths) {
        return target.select(paths);
    }

    @Override
    public Where0<T, Tuple> select(ExpressionsBuilder<T> selectBuilder) {
        return target.select(selectBuilder);
    }

    @Override
    public <R> Where0<T, R> select(Expression<T, R> expression) {
        return target.select(expression);
    }

    @Override
    public <R> Where0<T, R> select(Path<T, ? extends R> path) {
        return target.select(path);
    }

    @Override
    public Where0<T, Tuple> select(Collection<Path<T, ?>> paths) {
        return target.select(paths);
    }

    @Override
    public <A, B> Where0<T, Tuple2<A, B>> select(Path<T, A> a, Path<T, B> b) {
        return target.select(a, b);
    }

    @Override
    public <A, B, C> Where0<T, Tuple3<A, B, C>> select(Path<T, A> a, Path<T, B> b, Path<T, C> c) {
        return target.select(a, b, c);
    }

    @Override
    public <A, B, C, D> Where0<T, Tuple4<A, B, C, D>> select(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d) {
        return target.select(a, b, c, d);
    }

    @Override
    public <A, B, C, D, E> Where0<T, Tuple5<A, B, C, D, E>> select(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e) {
        return target.select(a, b, c, d, e);
    }

    @Override
    public <A, B, C, D, E, F> Where0<T, Tuple6<A, B, C, D, E, F>> select(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f) {
        return target.select(a, b, c, d, e, f);
    }

    @Override
    public <A, B, C, D, E, F, G> Where0<T, Tuple7<A, B, C, D, E, F, G>> select(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f, Path<T, G> g) {
        return target.select(a, b, c, d, e, f, g);
    }

    @Override
    public <A, B, C, D, E, F, G, H> Where0<T, Tuple8<A, B, C, D, E, F, G, H>> select(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f, Path<T, G> g, Path<T, H> h) {
        return target.select(a, b, c, d, e, f, g, h);
    }

    @Override
    public <A, B, C, D, E, F, G, H, I> Where0<T, Tuple9<A, B, C, D, E, F, G, H, I>> select(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f, Path<T, G> g, Path<T, H> h, Path<T, I> i) {
        return target.select(a, b, c, d, e, f, g, h, i);
    }

    @Override
    public <A, B, C, D, E, F, G, H, I, J> Where0<T, Tuple10<A, B, C, D, E, F, G, H, I, J>> select(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f, Path<T, G> g, Path<T, H> h, Path<T, I> i, Path<T, J> j) {
        return target.select(a, b, c, d, e, f, g, h, i, j);
    }

    @Override
    public <A, B> Where0<T, Tuple2<A, B>> select(Expression<T, A> a, Expression<T, B> b) {
        return target.select(a, b);
    }

    @Override
    public <A, B, C> Where0<T, Tuple3<A, B, C>> select(Expression<T, A> a, Expression<T, B> b, Expression<T, C> c) {
        return target.select(a, b, c);
    }

    @Override
    public <A, B, C, D> Where0<T, Tuple4<A, B, C, D>> select(Expression<T, A> a, Expression<T, B> b, Expression<T, C> c, Expression<T, D> d) {
        return target.select(a, b, c, d);
    }

    @Override
    public <A, B, C, D, E> Where0<T, Tuple5<A, B, C, D, E>> select(Expression<T, A> a, Expression<T, B> b, Expression<T, C> c, Expression<T, D> d, Expression<T, E> e) {
        return target.select(a, b, c, d, e);
    }

    @Override
    public <A, B, C, D, E, F> Where0<T, Tuple6<A, B, C, D, E, F>> select(Expression<T, A> a, Expression<T, B> b, Expression<T, C> c, Expression<T, D> d, Expression<T, E> e, Expression<T, F> f) {
        return target.select(a, b, c, d, e, f);
    }

    @Override
    public <A, B, C, D, E, F, G> Where0<T, Tuple7<A, B, C, D, E, F, G>> select(Expression<T, A> a, Expression<T, B> b, Expression<T, C> c, Expression<T, D> d, Expression<T, E> e, Expression<T, F> f, Expression<T, G> g) {
        return target.select(a, b, c, d, e, f, g);
    }

    @Override
    public <A, B, C, D, E, F, G, H> Where0<T, Tuple8<A, B, C, D, E, F, G, H>> select(Expression<T, A> a, Expression<T, B> b, Expression<T, C> c, Expression<T, D> d, Expression<T, E> e, Expression<T, F> f, Expression<T, G> g, Expression<T, H> h) {
        return target.select(a, b, c, d, e, f, g, h);
    }

    @Override
    public <A, B, C, D, E, F, G, H, I> Where0<T, Tuple9<A, B, C, D, E, F, G, H, I>> select(Expression<T, A> a, Expression<T, B> b, Expression<T, C> c, Expression<T, D> d, Expression<T, E> e, Expression<T, F> f, Expression<T, G> g, Expression<T, H> h, Expression<T, I> i) {
        return target.select(a, b, c, d, e, f, g, h, i);
    }

    @Override
    public <A, B, C, D, E, F, G, H, I, J> Where0<T, Tuple10<A, B, C, D, E, F, G, H, I, J>> select(Expression<T, A> a, Expression<T, B> b, Expression<T, C> c, Expression<T, D> d, Expression<T, E> e, Expression<T, F> f, Expression<T, G> g, Expression<T, H> h, Expression<T, I> i, Expression<T, J> j) {
        return target.select(a, b, c, d, e, f, g, h, i, j);
    }

    @Override
    public <R> Where<T, R> selectDistinct(Class<R> projectionType) {
        return target.selectDistinct(projectionType);
    }

    @Override
    public Where0<T, Tuple> selectDistinct(List<? extends Expression<T, ?>> paths) {
        return target.selectDistinct(paths);
    }

    @Override
    public Where0<T, Tuple> selectDistinct(ExpressionsBuilder<T> selectBuilder) {
        return target.selectDistinct(selectBuilder);
    }

    @Override
    public <R> Where0<T, R> selectDistinct(Expression<T, R> expression) {
        return target.selectDistinct(expression);
    }

    @Override
    public <R> Where0<T, R> selectDistinct(Path<T, ? extends R> path) {
        return target.selectDistinct(path);
    }

    @Override
    public Where0<T, Tuple> selectDistinct(Collection<Path<T, ?>> paths) {
        return target.selectDistinct(paths);
    }

    @Override
    public <A, B> Where0<T, Tuple2<A, B>> selectDistinct(Path<T, A> a, Path<T, B> b) {
        return target.selectDistinct(a, b);
    }

    @Override
    public <A, B, C> Where0<T, Tuple3<A, B, C>> selectDistinct(Path<T, A> a, Path<T, B> b, Path<T, C> c) {
        return target.selectDistinct(a, b, c);
    }

    @Override
    public <A, B, C, D> Where0<T, Tuple4<A, B, C, D>> selectDistinct(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d) {
        return target.selectDistinct(a, b, c, d);
    }

    @Override
    public <A, B, C, D, E> Where0<T, Tuple5<A, B, C, D, E>> selectDistinct(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e) {
        return target.selectDistinct(a, b, c, d, e);
    }

    @Override
    public <A, B, C, D, E, F> Where0<T, Tuple6<A, B, C, D, E, F>> selectDistinct(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f) {
        return target.selectDistinct(a, b, c, d, e, f);
    }

    @Override
    public <A, B, C, D, E, F, G> Where0<T, Tuple7<A, B, C, D, E, F, G>> selectDistinct(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f, Path<T, G> g) {
        return target.selectDistinct(a, b, c, d, e, f, g);
    }

    @Override
    public <A, B, C, D, E, F, G, H> Where0<T, Tuple8<A, B, C, D, E, F, G, H>> selectDistinct(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f, Path<T, G> g, Path<T, H> h) {
        return target.selectDistinct(a, b, c, d, e, f, g, h);
    }

    @Override
    public <A, B, C, D, E, F, G, H, I> Where0<T, Tuple9<A, B, C, D, E, F, G, H, I>> selectDistinct(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f, Path<T, G> g, Path<T, H> h, Path<T, I> i) {
        return target.selectDistinct(a, b, c, d, e, f, g, h, i);
    }

    @Override
    public <A, B, C, D, E, F, G, H, I, J> Where0<T, Tuple10<A, B, C, D, E, F, G, H, I, J>> selectDistinct(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f, Path<T, G> g, Path<T, H> h, Path<T, I> i, Path<T, J> j) {
        return target.selectDistinct(a, b, c, d, e, f, g, h, i, j);
    }

    @Override
    public <A, B> Where0<T, Tuple2<A, B>> selectDistinct(Expression<T, A> a, Expression<T, B> b) {
        return target.selectDistinct(a, b);
    }

    @Override
    public <A, B, C> Where0<T, Tuple3<A, B, C>> selectDistinct(Expression<T, A> a, Expression<T, B> b, Expression<T, C> c) {
        return target.selectDistinct(a, b, c);
    }

    @Override
    public <A, B, C, D> Where0<T, Tuple4<A, B, C, D>> selectDistinct(Expression<T, A> a, Expression<T, B> b, Expression<T, C> c, Expression<T, D> d) {
        return target.selectDistinct(a, b, c, d);
    }

    @Override
    public <A, B, C, D, E> Where0<T, Tuple5<A, B, C, D, E>> selectDistinct(Expression<T, A> a, Expression<T, B> b, Expression<T, C> c, Expression<T, D> d, Expression<T, E> e) {
        return target.selectDistinct(a, b, c, d, e);
    }

    @Override
    public <A, B, C, D, E, F> Where0<T, Tuple6<A, B, C, D, E, F>> selectDistinct(Expression<T, A> a, Expression<T, B> b, Expression<T, C> c, Expression<T, D> d, Expression<T, E> e, Expression<T, F> f) {
        return target.selectDistinct(a, b, c, d, e, f);
    }

    @Override
    public <A, B, C, D, E, F, G> Where0<T, Tuple7<A, B, C, D, E, F, G>> selectDistinct(Expression<T, A> a, Expression<T, B> b, Expression<T, C> c, Expression<T, D> d, Expression<T, E> e, Expression<T, F> f, Expression<T, G> g) {
        return target.selectDistinct(a, b, c, d, e, f, g);
    }

    @Override
    public <A, B, C, D, E, F, G, H> Where0<T, Tuple8<A, B, C, D, E, F, G, H>> selectDistinct(Expression<T, A> a, Expression<T, B> b, Expression<T, C> c, Expression<T, D> d, Expression<T, E> e, Expression<T, F> f, Expression<T, G> g, Expression<T, H> h) {
        return target.selectDistinct(a, b, c, d, e, f, g, h);
    }

    @Override
    public <A, B, C, D, E, F, G, H, I> Where0<T, Tuple9<A, B, C, D, E, F, G, H, I>> selectDistinct(Expression<T, A> a, Expression<T, B> b, Expression<T, C> c, Expression<T, D> d, Expression<T, E> e, Expression<T, F> f, Expression<T, G> g, Expression<T, H> h, Expression<T, I> i) {
        return target.selectDistinct(a, b, c, d, e, f, g, h, i);
    }

    @Override
    public <A, B, C, D, E, F, G, H, I, J> Where0<T, Tuple10<A, B, C, D, E, F, G, H, I, J>> selectDistinct(Expression<T, A> a, Expression<T, B> b, Expression<T, C> c, Expression<T, D> d, Expression<T, E> e, Expression<T, F> f, Expression<T, G> g, Expression<T, H> h, Expression<T, I> i, Expression<T, J> j) {
        return target.selectDistinct(a, b, c, d, e, f, g, h, i, j);
    }

    @Override
    public Where<T, T> fetch(List<PathExpression<T, ?>> pathExpressions) {
        return target.fetch(pathExpressions);
    }

    @Override
    public Where<T, T> fetch(PathExpression<T, ?> path) {
        return target.fetch(path);
    }

    @Override
    public Where<T, T> fetch(PathExpression<T, ?> p0, PathExpression<T, ?> p1) {
        return target.fetch(p0, p1);
    }

    @Override
    public Where<T, T> fetch(PathExpression<T, ?> p0, PathExpression<T, ?> p1, PathExpression<T, ?> p3) {
        return target.fetch(p0, p1, p3);
    }

    @Override
    public Where<T, T> fetch(Collection<Path<T, ?>> paths) {
        return target.fetch(paths);
    }

    @Override
    public Where<T, T> fetch(Path<T, ?> path) {
        return target.fetch(path);
    }

    @Override
    public Where<T, T> fetch(Path<T, ?> p0, Path<T, ?> p1) {
        return target.fetch(p0, p1);
    }

    @Override
    public Where<T, T> fetch(Path<T, ?> p0, Path<T, ?> p1, Path<T, ?> p3) {
        return target.fetch(p0, p1, p3);
    }

    @Override
    public Where<T, T> where(Expression<T, Boolean> predicate) {
        return target.where(predicate);
    }

    @Override
    public Where<T, T> where(PredicateBuilder<T> predicateBuilder) {
        return target.where(predicateBuilder);
    }

    @Override
    public Where<T, T> whereIf(boolean predicate, PredicateBuilder<T> predicateBuilder) {
        return target.whereIf(predicate, predicateBuilder);
    }

    @Override
    public <N> PathOperator<T, N, ? extends Where<T, T>> where(Path<T, N> path) {
        return target.where(path);
    }

    @Override
    public <N extends Number> NumberOperator<T, N, ? extends Where<T, T>> where(NumberPath<T, N> path) {
        return target.where(path);
    }

    @Override
    public StringOperator<T, ? extends Where<T, T>> where(StringPath<T> path) {
        return target.where(path);
    }

    @Override
    public Collector<T> orderBy(List<? extends Order<T>> orders) {
        return target.orderBy(orders);
    }

    @Override
    public Collector<T> orderBy(Function<EntityRoot<T>, List<? extends Order<T>>> ordersBuilder) {
        return target.orderBy(ordersBuilder);
    }

    @Override
    public Collector<T> orderBy(Order<T> order) {
        return target.orderBy(order);
    }

    @Override
    public Collector<T> orderBy(Order<T> p0, Order<T> p1) {
        return target.orderBy(p0, p1);
    }

    @Override
    public Collector<T> orderBy(Order<T> order1, Order<T> order2, Order<T> order3) {
        return target.orderBy(order1, order2, order3);
    }

    @Override
    public OrderOperator<T, T> orderBy(Collection<Path<T, Comparable<?>>> paths) {
        return target.orderBy(paths);
    }

    @Override
    public OrderOperator<T, T> orderBy(Path<T, Comparable<?>> path) {
        return target.orderBy(path);
    }

    @Override
    public OrderOperator<T, T> orderBy(Path<T, Comparable<?>> p1, Path<T, Comparable<?>> p2) {
        return target.orderBy(p1, p2);
    }

    @Override
    public OrderOperator<T, T> orderBy(Path<T, Comparable<?>> p1, Path<T, Comparable<?>> p2, Path<T, Comparable<?>> p3) {
        return target.orderBy(p1, p2, p3);
    }

    @Override
    public EntityRoot<T> root() {
        return target.root();
    }

    @Override
    public long count() {
        return target.count();
    }

    @Override
    public List<T> getList(int offset, int maxResult, LockModeType lockModeType) {
        return target.getList(offset, maxResult, lockModeType);
    }

    @Override
    public List<T> getList(int offset, int maxResult) {
        return target.getList(offset, maxResult);
    }

    @Override
    public List<T> offset(int offset) {
        return Entities.super.offset(offset);
    }

    @Override
    public List<T> limit(int limit) {
        return Entities.super.limit(limit);
    }

    @Override
    public boolean exist(int offset) {
        return target.exist(offset);
    }

    @Override
    public Optional<T> first() {
        return target.first();
    }

    @Override
    public Optional<T> first(int offset) {
        return target.first(offset);
    }

    @Override
    public T getFirst() {
        return target.getFirst();
    }

    @Override
    public T getFirst(int offset) {
        return target.getFirst(offset);
    }

    @Override
    public T requireSingle() {
        return target.requireSingle();
    }

    @Override
    public Optional<T> single() {
        return target.single();
    }

    @Override
    public Optional<T> single(int offset) {
        return target.single(offset);
    }

    @Override
    public T getSingle() {
        return target.getSingle();
    }

    @Override
    public T getSingle(int offset) {
        return target.getSingle(offset);
    }

    @Override
    public List<T> getList() {
        return target.getList();
    }

    @Override
    public boolean exist() {
        return target.exist();
    }

    @Override
    public Optional<T> first(LockModeType lockModeType) {
        return target.first(lockModeType);
    }

    @Override
    public Optional<T> first(int offset, LockModeType lockModeType) {
        return target.first(offset, lockModeType);
    }

    @Override
    public T getFirst(LockModeType lockModeType) {
        return target.getFirst(lockModeType);
    }

    @Override
    public T getFirst(int offset, LockModeType lockModeType) {
        return target.getFirst(offset, lockModeType);
    }

    @Override
    public T requireSingle(LockModeType lockModeType) {
        return target.requireSingle(lockModeType);
    }

    @Override
    public Optional<T> single(LockModeType lockModeType) {
        return target.single(lockModeType);
    }

    @Override
    public Optional<T> single(int offset, LockModeType lockModeType) {
        return target.single(offset, lockModeType);
    }

    @Override
    public T getSingle(LockModeType lockModeType) {
        return target.getSingle(lockModeType);
    }

    @Override
    public T getSingle(int offset, LockModeType lockModeType) {
        return target.getSingle(offset, lockModeType);
    }

    @Override
    public List<T> offset(int offset, LockModeType lockModeType) {
        return target.offset(offset, lockModeType);
    }

    @Override
    public List<T> limit(int limit, LockModeType lockModeType) {
        return Entities.super.limit(limit, lockModeType);
    }

    @Override
    public List<T> getList(LockModeType lockModeType) {
        return target.getList(lockModeType);
    }

    @Override
    public <R> R slice(Sliceable<T, R> sliceable) {
        return target.slice(sliceable);
    }

    @Override
    public Slice<T> slice(int offset, int limit) {
        return target.slice(offset, limit);
    }

    @Override
    public QueryStructureBuilder buildMetadata() {
        return target.buildMetadata();
    }

    @Override
    public <X> SubQueryBuilder<X, T> asSubQuery() {
        return target.asSubQuery();
    }

    @Override
    public Page<T> getPage(Pageable pageable) {
        return target.getPage(pageable);
    }

    @Override
    public T insert(@NotNull T entity) {
        return target.insert(entity);
    }

    @Override
    public List<T> insert(@NotNull Iterable<T> entities) {
        return target.insert(entities);
    }

    @Override
    public List<T> update(@NotNull Iterable<T> entities) {
        return target.update(entities);
    }

    @Override
    public T update(@NotNull T entity) {
        return target.update(entity);
    }

    @Override
    public void delete(@NotNull Iterable<T> entities) {
        target.delete(entities);
    }

    @Override
    public void delete(@NotNull T entity) {
        target.delete(entity);
    }

    @Override
    public T updateNonNullColumn(@NotNull T entity) {
        return target.updateNonNullColumn(entity);
    }

    @Override
    public <U> PathExpression<T, U> path(String fieldName) {
        return target.path(fieldName);
    }

    @Override
    public <U> EntityPathExpression<T, U> entityPath(String fieldName) {
        return target.entityPath(fieldName);
    }

    @Override
    public StringPathExpression<T> stringPath(String fieldName) {
        return target.stringPath(fieldName);
    }

    @Override
    public <U extends Number> NumberPathExpression<T, U> numberPath(String fieldName) {
        return target.numberPath(fieldName);
    }

    @Override
    public BooleanPathExpression<T> booleanPath(String fieldName) {
        return target.booleanPath(fieldName);
    }
}
