package io.github.nextentity.core;

import io.github.nextentity.api.Collector;
import io.github.nextentity.api.ExpressionBuilder.NumberOperator;
import io.github.nextentity.api.ExpressionBuilder.PathOperator;
import io.github.nextentity.api.ExpressionBuilder.StringOperator;
import io.github.nextentity.api.OrderOperator;
import io.github.nextentity.api.Path;
import io.github.nextentity.api.Path.BooleanPath;
import io.github.nextentity.api.Path.NumberPath;
import io.github.nextentity.api.Path.StringPath;
import io.github.nextentity.api.Repository;
import io.github.nextentity.api.SubQueryBuilder;
import io.github.nextentity.api.TypedExpression;
import io.github.nextentity.api.TypedExpression.BooleanPathExpression;
import io.github.nextentity.api.TypedExpression.EntityPathExpression;
import io.github.nextentity.api.TypedExpression.NumberPathExpression;
import io.github.nextentity.api.TypedExpression.PathExpression;
import io.github.nextentity.api.TypedExpression.StringPathExpression;
import io.github.nextentity.api.SelectWhereStep;
import io.github.nextentity.api.RowsSelectWhereStep;
import io.github.nextentity.api.model.EntityRoot;
import io.github.nextentity.api.model.LockModeType;
import io.github.nextentity.api.model.Order;
import io.github.nextentity.api.model.Page;
import io.github.nextentity.api.model.Pageable;
import io.github.nextentity.api.model.Slice;
import io.github.nextentity.api.model.Sliceable;
import io.github.nextentity.api.model.Tuple;
import io.github.nextentity.api.model.Tuple10;
import io.github.nextentity.api.model.Tuple2;
import io.github.nextentity.api.model.Tuple3;
import io.github.nextentity.api.model.Tuple4;
import io.github.nextentity.api.model.Tuple5;
import io.github.nextentity.api.model.Tuple6;
import io.github.nextentity.api.model.Tuple7;
import io.github.nextentity.api.model.Tuple8;
import io.github.nextentity.api.model.Tuple9;
import org.jetbrains.annotations.NotNull;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;

/**
 * @author HuangChengwei
 * @since 2024-04-08 15:08
 */
public class RepositoryFaced<ID extends Serializable, T> implements Repository<ID, T> {

    private final Repository<ID, T> target;

    public RepositoryFaced(Repository<ID, T> target) {
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
    public <U> TypedExpression<T, U> literal(U value) {
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
    public <R> SelectWhereStep<T, R> select(Class<R> projectionType) {
        return target.select(projectionType);
    }

    @Override
    public RowsSelectWhereStep<T, Tuple> select(List<? extends TypedExpression<T, ?>> paths) {
        return target.select(paths);
    }

    @Override
    public <R> RowsSelectWhereStep<T, R> select(TypedExpression<T, R> expression) {
        return target.select(expression);
    }

    @Override
    public <R> RowsSelectWhereStep<T, R> select(Path<T, ? extends R> path) {
        return target.select(path);
    }

    @Override
    public RowsSelectWhereStep<T, Tuple> select(Collection<Path<T, ?>> paths) {
        return target.select(paths);
    }

    @Override
    public <A, B> RowsSelectWhereStep<T, Tuple2<A, B>> select(Path<T, A> a, Path<T, B> b) {
        return target.select(a, b);
    }

    @Override
    public <A, B, C> RowsSelectWhereStep<T, Tuple3<A, B, C>> select(Path<T, A> a, Path<T, B> b, Path<T, C> c) {
        return target.select(a, b, c);
    }

    @Override
    public <A, B, C, D> RowsSelectWhereStep<T, Tuple4<A, B, C, D>> select(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d) {
        return target.select(a, b, c, d);
    }

    @Override
    public <A, B, C, D, E> RowsSelectWhereStep<T, Tuple5<A, B, C, D, E>> select(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e) {
        return target.select(a, b, c, d, e);
    }

    @Override
    public <A, B, C, D, E, F> RowsSelectWhereStep<T, Tuple6<A, B, C, D, E, F>> select(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f) {
        return target.select(a, b, c, d, e, f);
    }

    @Override
    public <A, B, C, D, E, F, G> RowsSelectWhereStep<T, Tuple7<A, B, C, D, E, F, G>> select(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f, Path<T, G> g) {
        return target.select(a, b, c, d, e, f, g);
    }

    @Override
    public <A, B, C, D, E, F, G, H> RowsSelectWhereStep<T, Tuple8<A, B, C, D, E, F, G, H>> select(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f, Path<T, G> g, Path<T, H> h) {
        return target.select(a, b, c, d, e, f, g, h);
    }

    @Override
    public <A, B, C, D, E, F, G, H, I> RowsSelectWhereStep<T, Tuple9<A, B, C, D, E, F, G, H, I>> select(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f, Path<T, G> g, Path<T, H> h, Path<T, I> i) {
        return target.select(a, b, c, d, e, f, g, h, i);
    }

    @Override
    public <A, B, C, D, E, F, G, H, I, J> RowsSelectWhereStep<T, Tuple10<A, B, C, D, E, F, G, H, I, J>> select(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f, Path<T, G> g, Path<T, H> h, Path<T, I> i, Path<T, J> j) {
        return target.select(a, b, c, d, e, f, g, h, i, j);
    }

    @Override
    public <A, B> RowsSelectWhereStep<T, Tuple2<A, B>> select(TypedExpression<T, A> a, TypedExpression<T, B> b) {
        return target.select(a, b);
    }

    @Override
    public <A, B, C> RowsSelectWhereStep<T, Tuple3<A, B, C>> select(TypedExpression<T, A> a, TypedExpression<T, B> b, TypedExpression<T, C> c) {
        return target.select(a, b, c);
    }

    @Override
    public <A, B, C, D> RowsSelectWhereStep<T, Tuple4<A, B, C, D>> select(TypedExpression<T, A> a, TypedExpression<T, B> b, TypedExpression<T, C> c, TypedExpression<T, D> d) {
        return target.select(a, b, c, d);
    }

    @Override
    public <A, B, C, D, E> RowsSelectWhereStep<T, Tuple5<A, B, C, D, E>> select(TypedExpression<T, A> a, TypedExpression<T, B> b, TypedExpression<T, C> c, TypedExpression<T, D> d, TypedExpression<T, E> e) {
        return target.select(a, b, c, d, e);
    }

    @Override
    public <A, B, C, D, E, F> RowsSelectWhereStep<T, Tuple6<A, B, C, D, E, F>> select(TypedExpression<T, A> a, TypedExpression<T, B> b, TypedExpression<T, C> c, TypedExpression<T, D> d, TypedExpression<T, E> e, TypedExpression<T, F> f) {
        return target.select(a, b, c, d, e, f);
    }

    @Override
    public <A, B, C, D, E, F, G> RowsSelectWhereStep<T, Tuple7<A, B, C, D, E, F, G>> select(TypedExpression<T, A> a, TypedExpression<T, B> b, TypedExpression<T, C> c, TypedExpression<T, D> d, TypedExpression<T, E> e, TypedExpression<T, F> f, TypedExpression<T, G> g) {
        return target.select(a, b, c, d, e, f, g);
    }

    @Override
    public <A, B, C, D, E, F, G, H> RowsSelectWhereStep<T, Tuple8<A, B, C, D, E, F, G, H>> select(TypedExpression<T, A> a, TypedExpression<T, B> b, TypedExpression<T, C> c, TypedExpression<T, D> d, TypedExpression<T, E> e, TypedExpression<T, F> f, TypedExpression<T, G> g, TypedExpression<T, H> h) {
        return target.select(a, b, c, d, e, f, g, h);
    }

    @Override
    public <A, B, C, D, E, F, G, H, I> RowsSelectWhereStep<T, Tuple9<A, B, C, D, E, F, G, H, I>> select(TypedExpression<T, A> a, TypedExpression<T, B> b, TypedExpression<T, C> c, TypedExpression<T, D> d, TypedExpression<T, E> e, TypedExpression<T, F> f, TypedExpression<T, G> g, TypedExpression<T, H> h, TypedExpression<T, I> i) {
        return target.select(a, b, c, d, e, f, g, h, i);
    }

    @Override
    public <A, B, C, D, E, F, G, H, I, J> RowsSelectWhereStep<T, Tuple10<A, B, C, D, E, F, G, H, I, J>> select(TypedExpression<T, A> a, TypedExpression<T, B> b, TypedExpression<T, C> c, TypedExpression<T, D> d, TypedExpression<T, E> e, TypedExpression<T, F> f, TypedExpression<T, G> g, TypedExpression<T, H> h, TypedExpression<T, I> i, TypedExpression<T, J> j) {
        return target.select(a, b, c, d, e, f, g, h, i, j);
    }

    @Override
    public <R> SelectWhereStep<T, R> selectDistinct(Class<R> projectionType) {
        return target.selectDistinct(projectionType);
    }

    @Override
    public RowsSelectWhereStep<T, Tuple> selectDistinct(List<? extends TypedExpression<T, ?>> paths) {
        return target.selectDistinct(paths);
    }

    @Override
    public <R> RowsSelectWhereStep<T, R> selectDistinct(TypedExpression<T, R> expression) {
        return target.selectDistinct(expression);
    }

    @Override
    public <R> RowsSelectWhereStep<T, R> selectDistinct(Path<T, ? extends R> path) {
        return target.selectDistinct(path);
    }

    @Override
    public RowsSelectWhereStep<T, Tuple> selectDistinct(Collection<Path<T, ?>> paths) {
        return target.selectDistinct(paths);
    }

    @Override
    public <A, B> RowsSelectWhereStep<T, Tuple2<A, B>> selectDistinct(Path<T, A> a, Path<T, B> b) {
        return target.selectDistinct(a, b);
    }

    @Override
    public <A, B, C> RowsSelectWhereStep<T, Tuple3<A, B, C>> selectDistinct(Path<T, A> a, Path<T, B> b, Path<T, C> c) {
        return target.selectDistinct(a, b, c);
    }

    @Override
    public <A, B, C, D> RowsSelectWhereStep<T, Tuple4<A, B, C, D>> selectDistinct(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d) {
        return target.selectDistinct(a, b, c, d);
    }

    @Override
    public <A, B, C, D, E> RowsSelectWhereStep<T, Tuple5<A, B, C, D, E>> selectDistinct(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e) {
        return target.selectDistinct(a, b, c, d, e);
    }

    @Override
    public <A, B, C, D, E, F> RowsSelectWhereStep<T, Tuple6<A, B, C, D, E, F>> selectDistinct(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f) {
        return target.selectDistinct(a, b, c, d, e, f);
    }

    @Override
    public <A, B, C, D, E, F, G> RowsSelectWhereStep<T, Tuple7<A, B, C, D, E, F, G>> selectDistinct(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f, Path<T, G> g) {
        return target.selectDistinct(a, b, c, d, e, f, g);
    }

    @Override
    public <A, B, C, D, E, F, G, H> RowsSelectWhereStep<T, Tuple8<A, B, C, D, E, F, G, H>> selectDistinct(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f, Path<T, G> g, Path<T, H> h) {
        return target.selectDistinct(a, b, c, d, e, f, g, h);
    }

    @Override
    public <A, B, C, D, E, F, G, H, I> RowsSelectWhereStep<T, Tuple9<A, B, C, D, E, F, G, H, I>> selectDistinct(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f, Path<T, G> g, Path<T, H> h, Path<T, I> i) {
        return target.selectDistinct(a, b, c, d, e, f, g, h, i);
    }

    @Override
    public <A, B, C, D, E, F, G, H, I, J> RowsSelectWhereStep<T, Tuple10<A, B, C, D, E, F, G, H, I, J>> selectDistinct(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f, Path<T, G> g, Path<T, H> h, Path<T, I> i, Path<T, J> j) {
        return target.selectDistinct(a, b, c, d, e, f, g, h, i, j);
    }

    @Override
    public <A, B> RowsSelectWhereStep<T, Tuple2<A, B>> selectDistinct(TypedExpression<T, A> a, TypedExpression<T, B> b) {
        return target.selectDistinct(a, b);
    }

    @Override
    public <A, B, C> RowsSelectWhereStep<T, Tuple3<A, B, C>> selectDistinct(TypedExpression<T, A> a, TypedExpression<T, B> b, TypedExpression<T, C> c) {
        return target.selectDistinct(a, b, c);
    }

    @Override
    public <A, B, C, D> RowsSelectWhereStep<T, Tuple4<A, B, C, D>> selectDistinct(TypedExpression<T, A> a, TypedExpression<T, B> b, TypedExpression<T, C> c, TypedExpression<T, D> d) {
        return target.selectDistinct(a, b, c, d);
    }

    @Override
    public <A, B, C, D, E> RowsSelectWhereStep<T, Tuple5<A, B, C, D, E>> selectDistinct(TypedExpression<T, A> a, TypedExpression<T, B> b, TypedExpression<T, C> c, TypedExpression<T, D> d, TypedExpression<T, E> e) {
        return target.selectDistinct(a, b, c, d, e);
    }

    @Override
    public <A, B, C, D, E, F> RowsSelectWhereStep<T, Tuple6<A, B, C, D, E, F>> selectDistinct(TypedExpression<T, A> a, TypedExpression<T, B> b, TypedExpression<T, C> c, TypedExpression<T, D> d, TypedExpression<T, E> e, TypedExpression<T, F> f) {
        return target.selectDistinct(a, b, c, d, e, f);
    }

    @Override
    public <A, B, C, D, E, F, G> RowsSelectWhereStep<T, Tuple7<A, B, C, D, E, F, G>> selectDistinct(TypedExpression<T, A> a, TypedExpression<T, B> b, TypedExpression<T, C> c, TypedExpression<T, D> d, TypedExpression<T, E> e, TypedExpression<T, F> f, TypedExpression<T, G> g) {
        return target.selectDistinct(a, b, c, d, e, f, g);
    }

    @Override
    public <A, B, C, D, E, F, G, H> RowsSelectWhereStep<T, Tuple8<A, B, C, D, E, F, G, H>> selectDistinct(TypedExpression<T, A> a, TypedExpression<T, B> b, TypedExpression<T, C> c, TypedExpression<T, D> d, TypedExpression<T, E> e, TypedExpression<T, F> f, TypedExpression<T, G> g, TypedExpression<T, H> h) {
        return target.selectDistinct(a, b, c, d, e, f, g, h);
    }

    @Override
    public <A, B, C, D, E, F, G, H, I> RowsSelectWhereStep<T, Tuple9<A, B, C, D, E, F, G, H, I>> selectDistinct(TypedExpression<T, A> a, TypedExpression<T, B> b, TypedExpression<T, C> c, TypedExpression<T, D> d, TypedExpression<T, E> e, TypedExpression<T, F> f, TypedExpression<T, G> g, TypedExpression<T, H> h, TypedExpression<T, I> i) {
        return target.selectDistinct(a, b, c, d, e, f, g, h, i);
    }

    @Override
    public <A, B, C, D, E, F, G, H, I, J> RowsSelectWhereStep<T, Tuple10<A, B, C, D, E, F, G, H, I, J>> selectDistinct(TypedExpression<T, A> a, TypedExpression<T, B> b, TypedExpression<T, C> c, TypedExpression<T, D> d, TypedExpression<T, E> e, TypedExpression<T, F> f, TypedExpression<T, G> g, TypedExpression<T, H> h, TypedExpression<T, I> i, TypedExpression<T, J> j) {
        return target.selectDistinct(a, b, c, d, e, f, g, h, i, j);
    }

    @Override
    public SelectWhereStep<T, T> fetch(List<PathExpression<T, ?>> pathExpressions) {
        return target.fetch(pathExpressions);
    }

    @Override
    public SelectWhereStep<T, T> fetch(PathExpression<T, ?> path) {
        return target.fetch(path);
    }

    @Override
    public SelectWhereStep<T, T> fetch(PathExpression<T, ?> p0, PathExpression<T, ?> p1) {
        return target.fetch(p0, p1);
    }

    @Override
    public SelectWhereStep<T, T> fetch(PathExpression<T, ?> p0, PathExpression<T, ?> p1, PathExpression<T, ?> p3) {
        return target.fetch(p0, p1, p3);
    }

    @Override
    public SelectWhereStep<T, T> fetch(Collection<Path<T, ?>> paths) {
        return target.fetch(paths);
    }

    @Override
    public SelectWhereStep<T, T> fetch(Path<T, ?> path) {
        return target.fetch(path);
    }

    @Override
    public SelectWhereStep<T, T> fetch(Path<T, ?> p0, Path<T, ?> p1) {
        return target.fetch(p0, p1);
    }

    @Override
    public SelectWhereStep<T, T> fetch(Path<T, ?> p0, Path<T, ?> p1, Path<T, ?> p3) {
        return target.fetch(p0, p1, p3);
    }

    @Override
    public SelectWhereStep<T, T> where(TypedExpression<T, Boolean> predicate) {
        return target.where(predicate);
    }

    @Override
    public <N> PathOperator<T, N, ? extends SelectWhereStep<T, T>> where(Path<T, N> path) {
        return target.where(path);
    }

    @Override
    public <N extends Number> NumberOperator<T, N, ? extends SelectWhereStep<T, T>> where(NumberPath<T, N> path) {
        return target.where(path);
    }

    @Override
    public StringOperator<T, ? extends SelectWhereStep<T, T>> where(StringPath<T> path) {
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
        return Repository.super.offset(offset);
    }

    @Override
    public List<T> limit(int limit) {
        return Repository.super.limit(limit);
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
        return Repository.super.limit(limit, lockModeType);
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
    public <X> SubQueryBuilder<X, T> asSubQuery() {
        return target.asSubQuery();
    }

    @Override
    public <R> Collector<R> map(Function<? super T, ? extends R> mapper) {
        return target.map(mapper);
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
    public List<T> insert(@NotNull Iterable<T> Repository) {
        return target.insert(Repository);
    }

    @Override
    public List<T> update(@NotNull Iterable<T> Repository) {
        return target.update(Repository);
    }

    @Override
    public T update(@NotNull T entity) {
        return target.update(entity);
    }

    @Override
    public void delete(@NotNull Iterable<T> Repository) {
        target.delete(Repository);
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
