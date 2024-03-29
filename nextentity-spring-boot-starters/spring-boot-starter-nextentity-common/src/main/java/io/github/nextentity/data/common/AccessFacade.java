package io.github.nextentity.data.common;

import io.github.nextentity.core.Expressions;
import io.github.nextentity.core.TypeCastUtil;
import io.github.nextentity.core.TypedExpressions;
import io.github.nextentity.core.api.ExpressionOperator.ComparableOperator;
import io.github.nextentity.core.api.ExpressionOperator.NumberOperator;
import io.github.nextentity.core.api.ExpressionOperator.PathOperator;
import io.github.nextentity.core.api.ExpressionOperator.StringOperator;
import io.github.nextentity.core.api.LockModeType;
import io.github.nextentity.core.api.Order;
import io.github.nextentity.core.api.Path;
import io.github.nextentity.core.api.Path.ComparablePath;
import io.github.nextentity.core.api.Path.NumberPath;
import io.github.nextentity.core.api.Path.StringPath;
import io.github.nextentity.core.api.Query;
import io.github.nextentity.core.api.Query.Collector;
import io.github.nextentity.core.api.Query.ExpressionsBuilder;
import io.github.nextentity.core.api.Query.OrderOperator;
import io.github.nextentity.core.api.Query.QueryStructureBuilder;
import io.github.nextentity.core.api.Query.Select;
import io.github.nextentity.core.api.Query.SubQueryBuilder;
import io.github.nextentity.core.api.Query.Where;
import io.github.nextentity.core.api.Query.Where0;
import io.github.nextentity.core.api.Root;
import io.github.nextentity.core.api.Slice;
import io.github.nextentity.core.api.Sliceable;
import io.github.nextentity.core.api.TypedExpression;
import io.github.nextentity.core.api.TypedExpression.BasicExpression;
import io.github.nextentity.core.api.TypedExpression.BooleanExpression;
import io.github.nextentity.core.api.TypedExpression.PathExpression;
import io.github.nextentity.core.api.Update;
import io.github.nextentity.core.api.Updater;
import io.github.nextentity.core.meta.Attribute;
import io.github.nextentity.core.meta.Metamodel;
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

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

public class AccessFacade<T, ID> implements Access<T, ID> {

    protected Select<T> select;
    protected Updater<T> updater;

    protected BasicExpression<T, ID> id;
    protected Function<T, ID> getId;

    protected void init(Class<T> entityType, Query query, Update update, Metamodel metamodel) {
        this.select = query.from(entityType);
        this.updater = update.getUpdater(entityType);
        Attribute idAttribute = metamodel.getEntity(entityType).id();
        this.id = TypedExpressions.ofBasic(Expressions.column(idAttribute.name()));
        this.getId = t -> TypeCastUtil.unsafeCast(idAttribute.get(t));
    }

    public T get(ID id) {
        return where(this.id.eq(id)).getSingle();
    }

    public List<T> getAll(Iterable<? extends ID> ids) {
        Collection<? extends ID> idList = ids instanceof Collection<?>
                ? (Collection<? extends ID>) ids
                : StreamSupport.stream(ids.spliterator(), false).collect(Collectors.toList());
        if (idList.isEmpty()) {
            return Collections.emptyList();
        }
        BooleanExpression<T> predicate = id.in(idList);
        return where(predicate).getList();
    }

    public Map<ID, T> getMap(Iterable<? extends ID> ids) {
        List<T> entities = getAll(ids);
        return entities.stream().collect(Collectors.toMap(getId, Function.identity()));
    }

    public <R> Where<T, R> select(Class<R> projectionType) {
        return select.select(projectionType);
    }

    public Where0<T, Tuple> select(List<? extends TypedExpression<T, ?>> paths) {
        return select.select(paths);
    }

    public Where0<T, Tuple> select(ExpressionsBuilder<T> selectBuilder) {
        return select.select(selectBuilder);
    }

    public <R> Where0<T, R> select(TypedExpression<T, R> expression) {
        return select.select(expression);
    }

    public <R> Where0<T, R> select(Path<T, ? extends R> path) {
        return select.select(path);
    }

    public Where0<T, Tuple> select(Collection<Path<T, ?>> paths) {
        return select.select(paths);
    }

    public <A, B> Where0<T, Tuple2<A, B>> select(Path<T, A> a, Path<T, B> b) {
        return select.select(a, b);
    }

    public <A, B, C> Where0<T, Tuple3<A, B, C>> select(Path<T, A> a, Path<T, B> b, Path<T, C> c) {
        return select.select(a, b, c);
    }

    public <A, B, C, D> Where0<T, Tuple4<A, B, C, D>> select(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d) {
        return select.select(a, b, c, d);
    }

    public <A, B, C, D, E> Where0<T, Tuple5<A, B, C, D, E>> select(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e) {
        return select.select(a, b, c, d, e);
    }

    public <A, B, C, D, E, F> Where0<T, Tuple6<A, B, C, D, E, F>> select(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f) {
        return select.select(a, b, c, d, e, f);
    }

    public <A, B, C, D, E, F, G> Where0<T, Tuple7<A, B, C, D, E, F, G>> select(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f, Path<T, G> g) {
        return select.select(a, b, c, d, e, f, g);
    }

    public <A, B, C, D, E, F, G, H> Where0<T, Tuple8<A, B, C, D, E, F, G, H>> select(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f, Path<T, G> g, Path<T, H> h) {
        return select.select(a, b, c, d, e, f, g, h);
    }

    public <A, B, C, D, E, F, G, H, I> Where0<T, Tuple9<A, B, C, D, E, F, G, H, I>> select(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f, Path<T, G> g, Path<T, H> h, Path<T, I> i) {
        return select.select(a, b, c, d, e, f, g, h, i);
    }

    public <A, B, C, D, E, F, G, H, I, J> Where0<T, Tuple10<A, B, C, D, E, F, G, H, I, J>> select(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f, Path<T, G> g, Path<T, H> h, Path<T, I> i, Path<T, J> j) {
        return select.select(a, b, c, d, e, f, g, h, i, j);
    }

    public <A, B> Where0<T, Tuple2<A, B>> select(TypedExpression<T, A> a, TypedExpression<T, B> b) {
        return select.select(a, b);
    }

    public <A, B, C> Where0<T, Tuple3<A, B, C>> select(TypedExpression<T, A> a, TypedExpression<T, B> b, TypedExpression<T, C> c) {
        return select.select(a, b, c);
    }

    public <A, B, C, D> Where0<T, Tuple4<A, B, C, D>> select(TypedExpression<T, A> a, TypedExpression<T, B> b, TypedExpression<T, C> c, TypedExpression<T, D> d) {
        return select.select(a, b, c, d);
    }

    public <A, B, C, D, E> Where0<T, Tuple5<A, B, C, D, E>> select(TypedExpression<T, A> a, TypedExpression<T, B> b, TypedExpression<T, C> c, TypedExpression<T, D> d, TypedExpression<T, E> e) {
        return select.select(a, b, c, d, e);
    }

    public <A, B, C, D, E, F> Where0<T, Tuple6<A, B, C, D, E, F>> select(TypedExpression<T, A> a, TypedExpression<T, B> b, TypedExpression<T, C> c, TypedExpression<T, D> d, TypedExpression<T, E> e, TypedExpression<T, F> f) {
        return select.select(a, b, c, d, e, f);
    }

    public <A, B, C, D, E, F, G> Where0<T, Tuple7<A, B, C, D, E, F, G>> select(TypedExpression<T, A> a, TypedExpression<T, B> b, TypedExpression<T, C> c, TypedExpression<T, D> d, TypedExpression<T, E> e, TypedExpression<T, F> f, TypedExpression<T, G> g) {
        return select.select(a, b, c, d, e, f, g);
    }

    public <A, B, C, D, E, F, G, H> Where0<T, Tuple8<A, B, C, D, E, F, G, H>> select(TypedExpression<T, A> a, TypedExpression<T, B> b, TypedExpression<T, C> c, TypedExpression<T, D> d, TypedExpression<T, E> e, TypedExpression<T, F> f, TypedExpression<T, G> g, TypedExpression<T, H> h) {
        return select.select(a, b, c, d, e, f, g, h);
    }

    public <A, B, C, D, E, F, G, H, I> Where0<T, Tuple9<A, B, C, D, E, F, G, H, I>> select(TypedExpression<T, A> a, TypedExpression<T, B> b, TypedExpression<T, C> c, TypedExpression<T, D> d, TypedExpression<T, E> e, TypedExpression<T, F> f, TypedExpression<T, G> g, TypedExpression<T, H> h, TypedExpression<T, I> i) {
        return select.select(a, b, c, d, e, f, g, h, i);
    }

    public <A, B, C, D, E, F, G, H, I, J> Where0<T, Tuple10<A, B, C, D, E, F, G, H, I, J>> select(TypedExpression<T, A> a, TypedExpression<T, B> b, TypedExpression<T, C> c, TypedExpression<T, D> d, TypedExpression<T, E> e, TypedExpression<T, F> f, TypedExpression<T, G> g, TypedExpression<T, H> h, TypedExpression<T, I> i, TypedExpression<T, J> j) {
        return select.select(a, b, c, d, e, f, g, h, i, j);
    }

    public <R> Where<T, R> selectDistinct(Class<R> projectionType) {
        return select.selectDistinct(projectionType);
    }

    public Where0<T, Tuple> selectDistinct(List<? extends TypedExpression<T, ?>> paths) {
        return select.selectDistinct(paths);
    }

    public Where0<T, Tuple> selectDistinct(ExpressionsBuilder<T> selectBuilder) {
        return select.selectDistinct(selectBuilder);
    }

    public <R> Where0<T, R> selectDistinct(TypedExpression<T, R> expression) {
        return select.selectDistinct(expression);
    }

    public <R> Where0<T, R> selectDistinct(Path<T, ? extends R> path) {
        return select.selectDistinct(path);
    }

    public Where0<T, Tuple> selectDistinct(Collection<Path<T, ?>> paths) {
        return select.selectDistinct(paths);
    }

    public <A, B> Where0<T, Tuple2<A, B>> selectDistinct(Path<T, A> a, Path<T, B> b) {
        return select.selectDistinct(a, b);
    }

    public <A, B, C> Where0<T, Tuple3<A, B, C>> selectDistinct(Path<T, A> a, Path<T, B> b, Path<T, C> c) {
        return select.selectDistinct(a, b, c);
    }

    public <A, B, C, D> Where0<T, Tuple4<A, B, C, D>> selectDistinct(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d) {
        return select.selectDistinct(a, b, c, d);
    }

    public <A, B, C, D, E> Where0<T, Tuple5<A, B, C, D, E>> selectDistinct(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e) {
        return select.selectDistinct(a, b, c, d, e);
    }

    public <A, B, C, D, E, F> Where0<T, Tuple6<A, B, C, D, E, F>> selectDistinct(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f) {
        return select.selectDistinct(a, b, c, d, e, f);
    }

    public <A, B, C, D, E, F, G> Where0<T, Tuple7<A, B, C, D, E, F, G>> selectDistinct(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f, Path<T, G> g) {
        return select.selectDistinct(a, b, c, d, e, f, g);
    }

    public <A, B, C, D, E, F, G, H> Where0<T, Tuple8<A, B, C, D, E, F, G, H>> selectDistinct(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f, Path<T, G> g, Path<T, H> h) {
        return select.selectDistinct(a, b, c, d, e, f, g, h);
    }

    public <A, B, C, D, E, F, G, H, I> Where0<T, Tuple9<A, B, C, D, E, F, G, H, I>> selectDistinct(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f, Path<T, G> g, Path<T, H> h, Path<T, I> i) {
        return select.selectDistinct(a, b, c, d, e, f, g, h, i);
    }

    public <A, B, C, D, E, F, G, H, I, J> Where0<T, Tuple10<A, B, C, D, E, F, G, H, I, J>> selectDistinct(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f, Path<T, G> g, Path<T, H> h, Path<T, I> i, Path<T, J> j) {
        return select.selectDistinct(a, b, c, d, e, f, g, h, i, j);
    }

    public <A, B> Where0<T, Tuple2<A, B>> selectDistinct(TypedExpression<T, A> a, TypedExpression<T, B> b) {
        return select.selectDistinct(a, b);
    }

    public <A, B, C> Where0<T, Tuple3<A, B, C>> selectDistinct(TypedExpression<T, A> a, TypedExpression<T, B> b, TypedExpression<T, C> c) {
        return select.selectDistinct(a, b, c);
    }

    public <A, B, C, D> Where0<T, Tuple4<A, B, C, D>> selectDistinct(TypedExpression<T, A> a, TypedExpression<T, B> b, TypedExpression<T, C> c, TypedExpression<T, D> d) {
        return select.selectDistinct(a, b, c, d);
    }

    public <A, B, C, D, E> Where0<T, Tuple5<A, B, C, D, E>> selectDistinct(TypedExpression<T, A> a, TypedExpression<T, B> b, TypedExpression<T, C> c, TypedExpression<T, D> d, TypedExpression<T, E> e) {
        return select.selectDistinct(a, b, c, d, e);
    }

    public <A, B, C, D, E, F> Where0<T, Tuple6<A, B, C, D, E, F>> selectDistinct(TypedExpression<T, A> a, TypedExpression<T, B> b, TypedExpression<T, C> c, TypedExpression<T, D> d, TypedExpression<T, E> e, TypedExpression<T, F> f) {
        return select.selectDistinct(a, b, c, d, e, f);
    }

    public <A, B, C, D, E, F, G> Where0<T, Tuple7<A, B, C, D, E, F, G>> selectDistinct(TypedExpression<T, A> a, TypedExpression<T, B> b, TypedExpression<T, C> c, TypedExpression<T, D> d, TypedExpression<T, E> e, TypedExpression<T, F> f, TypedExpression<T, G> g) {
        return select.selectDistinct(a, b, c, d, e, f, g);
    }

    public <A, B, C, D, E, F, G, H> Where0<T, Tuple8<A, B, C, D, E, F, G, H>> selectDistinct(TypedExpression<T, A> a, TypedExpression<T, B> b, TypedExpression<T, C> c, TypedExpression<T, D> d, TypedExpression<T, E> e, TypedExpression<T, F> f, TypedExpression<T, G> g, TypedExpression<T, H> h) {
        return select.selectDistinct(a, b, c, d, e, f, g, h);
    }

    public <A, B, C, D, E, F, G, H, I> Where0<T, Tuple9<A, B, C, D, E, F, G, H, I>> selectDistinct(TypedExpression<T, A> a, TypedExpression<T, B> b, TypedExpression<T, C> c, TypedExpression<T, D> d, TypedExpression<T, E> e, TypedExpression<T, F> f, TypedExpression<T, G> g, TypedExpression<T, H> h, TypedExpression<T, I> i) {
        return select.selectDistinct(a, b, c, d, e, f, g, h, i);
    }

    public <A, B, C, D, E, F, G, H, I, J> Where0<T, Tuple10<A, B, C, D, E, F, G, H, I, J>> selectDistinct(TypedExpression<T, A> a, TypedExpression<T, B> b, TypedExpression<T, C> c, TypedExpression<T, D> d, TypedExpression<T, E> e, TypedExpression<T, F> f, TypedExpression<T, G> g, TypedExpression<T, H> h, TypedExpression<T, I> i, TypedExpression<T, J> j) {
        return select.selectDistinct(a, b, c, d, e, f, g, h, i, j);
    }

    public Where<T, T> fetch(List<PathExpression<T, ?>> expressions) {
        return select.fetch(expressions);
    }

    public Where<T, T> fetch(PathExpression<T, ?> path) {
        return select.fetch(path);
    }

    public Where<T, T> fetch(PathExpression<T, ?> p0, PathExpression<T, ?> p1) {
        return select.fetch(p0, p1);
    }

    public Where<T, T> fetch(PathExpression<T, ?> p0, PathExpression<T, ?> p1, PathExpression<T, ?> p3) {
        return select.fetch(p0, p1, p3);
    }

    public Where<T, T> fetch(Collection<Path<T, ?>> paths) {
        return select.fetch(paths);
    }

    public Where<T, T> fetch(Path<T, ?> path) {
        return select.fetch(path);
    }

    public Where<T, T> fetch(Path<T, ?> p0, Path<T, ?> p1) {
        return select.fetch(p0, p1);
    }

    public Where<T, T> fetch(Path<T, ?> p0, Path<T, ?> p1, Path<T, ?> p3) {
        return select.fetch(p0, p1, p3);
    }

    public Where<T, T> where(TypedExpression<T, Boolean> predicate) {
        return select.where(predicate);
    }

    public <N> PathOperator<T, N, ? extends Where<T, T>> where(Path<T, N> path) {
        return select.where(path);
    }

    public <N extends Number & Comparable<N>> NumberOperator<T, N, ? extends Where<T, T>> where(NumberPath<T, N> path) {
        return select.where(path);
    }

    public <N extends Comparable<N>> ComparableOperator<T, N, ? extends Where<T, T>> where(ComparablePath<T, N> path) {
        return select.where(path);
    }

    public StringOperator<T, ? extends Where<T, T>> where(StringPath<T> path) {
        return select.where(path);
    }

    public Collector<T> orderBy(List<? extends Order<T>> orders) {
        return select.orderBy(orders);
    }

    public Collector<T> orderBy(Function<Root<T>, List<? extends Order<T>>> ordersBuilder) {
        return select.orderBy(ordersBuilder);
    }

    public Collector<T> orderBy(Order<T> order) {
        return select.orderBy(order);
    }

    public Collector<T> orderBy(Order<T> p0, Order<T> p1) {
        return select.orderBy(p0, p1);
    }

    public Collector<T> orderBy(Order<T> order1, Order<T> order2, Order<T> order3) {
        return select.orderBy(order1, order2, order3);
    }

    public OrderOperator<T, T> orderBy(Collection<Path<T, Comparable<?>>> paths) {
        return select.orderBy(paths);
    }

    public OrderOperator<T, T> orderBy(Path<T, Comparable<?>> path) {
        return select.orderBy(path);
    }

    public OrderOperator<T, T> orderBy(Path<T, Comparable<?>> p1, Path<T, Comparable<?>> p2) {
        return select.orderBy(p1, p2);
    }

    public OrderOperator<T, T> orderBy(Path<T, Comparable<?>> p1, Path<T, Comparable<?>> p2, Path<T, Comparable<?>> p3) {
        return select.orderBy(p1, p2, p3);
    }

    public long count() {
        return select.count();
    }

    public List<T> getList(int offset, int maxResult, LockModeType lockModeType) {
        return select.getList(offset, maxResult, lockModeType);
    }

    public List<T> getList(int offset, int maxResult) {
        return select.getList(offset, maxResult);
    }

    public boolean exist(int offset) {
        return select.exist(offset);
    }

    public Optional<T> first() {
        return select.first();
    }

    public Optional<T> first(int offset) {
        return select.first(offset);
    }

    public T getFirst() {
        return select.getFirst();
    }

    public T getFirst(int offset) {
        return select.getFirst(offset);
    }

    public T requireSingle() {
        return select.requireSingle();
    }

    public Optional<T> single() {
        return select.single();
    }

    public Optional<T> single(int offset) {
        return select.single(offset);
    }

    public T getSingle() {
        return select.getSingle();
    }

    public T getSingle(int offset) {
        return select.getSingle(offset);
    }

    public List<T> getList() {
        return select.getList();
    }

    public boolean exist() {
        return select.exist();
    }

    public Optional<T> first(LockModeType lockModeType) {
        return select.first(lockModeType);
    }

    public Optional<T> first(int offset, LockModeType lockModeType) {
        return select.first(offset, lockModeType);
    }

    public T getFirst(LockModeType lockModeType) {
        return select.getFirst(lockModeType);
    }

    public T getFirst(int offset, LockModeType lockModeType) {
        return select.getFirst(offset, lockModeType);
    }

    public T requireSingle(LockModeType lockModeType) {
        return select.requireSingle(lockModeType);
    }

    public Optional<T> single(LockModeType lockModeType) {
        return select.single(lockModeType);
    }

    public Optional<T> single(int offset, LockModeType lockModeType) {
        return select.single(offset, lockModeType);
    }

    public T getSingle(LockModeType lockModeType) {
        return select.getSingle(lockModeType);
    }

    public T getSingle(int offset, LockModeType lockModeType) {
        return select.getSingle(offset, lockModeType);
    }

    public List<T> getList(int offset, LockModeType lockModeType) {
        return select.getList(offset, lockModeType);
    }

    public List<T> getList(LockModeType lockModeType) {
        return select.getList(lockModeType);
    }

    public <R> R slice(Sliceable<T, R> sliceable) {
        return select.slice(sliceable);
    }

    public Slice<T> slice(int offset, int limit) {
        return select.slice(offset, limit);
    }

    public <X> SubQueryBuilder<X, T> asSubQuery() {
        return select.asSubQuery();
    }

    public QueryStructureBuilder buildMetadata() {
        return select.buildMetadata();
    }

    public Root<T> root() {
        return select.root();
    }

    public T insert(T entity) {
        return updater.insert(entity);
    }

    public List<T> insert(List<T> entities) {
        return updater.insert(entities);
    }

    public List<T> update(List<T> entities) {
        return updater.update(entities);
    }

    public T update(T entity) {
        return updater.update(entity);
    }

    public void delete(Iterable<T> entities) {
        updater.delete(entities);
    }

    public void delete(T entity) {
        updater.delete(entity);
    }

    public T updateNonNullColumn(T entity) {
        return updater.updateNonNullColumn(entity);
    }
}
