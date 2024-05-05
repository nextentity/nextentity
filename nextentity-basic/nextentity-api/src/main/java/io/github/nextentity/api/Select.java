package io.github.nextentity.api;

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

import java.util.Collection;
import java.util.List;

/**
 * @author HuangChengwei
 * @since 2024-05-06 8:38
 */
public
interface Select<T> extends SelectFetchStep<T> {

    <R> SelectWhereStep<T, R> select(Class<R> projectionType);

    RowsSelectWhereStep<T, Tuple> select(List<? extends TypedExpression<T, ?>> paths);

    <R> RowsSelectWhereStep<T, R> select(TypedExpression<T, R> expression);

    <R> RowsSelectWhereStep<T, R> select(Path<T, ? extends R> path);

    RowsSelectWhereStep<T, Tuple> select(Collection<Path<T, ?>> paths);

    <A, B> RowsSelectWhereStep<T, Tuple2<A, B>> select(Path<T, A> a, Path<T, B> b);

    <A, B, C> RowsSelectWhereStep<T, Tuple3<A, B, C>> select(Path<T, A> a, Path<T, B> b, Path<T, C> c);

    <A, B, C, D> RowsSelectWhereStep<T, Tuple4<A, B, C, D>> select(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d);

    <A, B, C, D, E> RowsSelectWhereStep<T, Tuple5<A, B, C, D, E>> select(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e);

    <A, B, C, D, E, F> RowsSelectWhereStep<T, Tuple6<A, B, C, D, E, F>> select(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f);

    <A, B, C, D, E, F, G> RowsSelectWhereStep<T, Tuple7<A, B, C, D, E, F, G>> select(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f, Path<T, G> g);

    <A, B, C, D, E, F, G, H> RowsSelectWhereStep<T, Tuple8<A, B, C, D, E, F, G, H>> select(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f, Path<T, G> g, Path<T, H> h);

    <A, B, C, D, E, F, G, H, I> RowsSelectWhereStep<T, Tuple9<A, B, C, D, E, F, G, H, I>> select(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f, Path<T, G> g, Path<T, H> h, Path<T, I> i);

    <A, B, C, D, E, F, G, H, I, J> RowsSelectWhereStep<T, Tuple10<A, B, C, D, E, F, G, H, I, J>> select(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f, Path<T, G> g, Path<T, H> h, Path<T, I> i, Path<T, J> j);

    <A, B> RowsSelectWhereStep<T, Tuple2<A, B>> select(TypedExpression<T, A> a, TypedExpression<T, B> b);

    <A, B, C> RowsSelectWhereStep<T, Tuple3<A, B, C>> select(TypedExpression<T, A> a, TypedExpression<T, B> b, TypedExpression<T, C> c);

    <A, B, C, D> RowsSelectWhereStep<T, Tuple4<A, B, C, D>> select(TypedExpression<T, A> a, TypedExpression<T, B> b, TypedExpression<T, C> c, TypedExpression<T, D> d);

    <A, B, C, D, E> RowsSelectWhereStep<T, Tuple5<A, B, C, D, E>> select(TypedExpression<T, A> a, TypedExpression<T, B> b, TypedExpression<T, C> c, TypedExpression<T, D> d, TypedExpression<T, E> e);

    <A, B, C, D, E, F> RowsSelectWhereStep<T, Tuple6<A, B, C, D, E, F>> select(TypedExpression<T, A> a, TypedExpression<T, B> b, TypedExpression<T, C> c, TypedExpression<T, D> d, TypedExpression<T, E> e, TypedExpression<T, F> f);

    <A, B, C, D, E, F, G> RowsSelectWhereStep<T, Tuple7<A, B, C, D, E, F, G>> select(TypedExpression<T, A> a, TypedExpression<T, B> b, TypedExpression<T, C> c, TypedExpression<T, D> d, TypedExpression<T, E> e, TypedExpression<T, F> f, TypedExpression<T, G> g);

    <A, B, C, D, E, F, G, H> RowsSelectWhereStep<T, Tuple8<A, B, C, D, E, F, G, H>> select(TypedExpression<T, A> a, TypedExpression<T, B> b, TypedExpression<T, C> c, TypedExpression<T, D> d, TypedExpression<T, E> e, TypedExpression<T, F> f, TypedExpression<T, G> g, TypedExpression<T, H> h);

    <A, B, C, D, E, F, G, H, I> RowsSelectWhereStep<T, Tuple9<A, B, C, D, E, F, G, H, I>> select(TypedExpression<T, A> a, TypedExpression<T, B> b, TypedExpression<T, C> c, TypedExpression<T, D> d, TypedExpression<T, E> e, TypedExpression<T, F> f, TypedExpression<T, G> g, TypedExpression<T, H> h, TypedExpression<T, I> i);

    <A, B, C, D, E, F, G, H, I, J> RowsSelectWhereStep<T, Tuple10<A, B, C, D, E, F, G, H, I, J>> select(TypedExpression<T, A> a, TypedExpression<T, B> b, TypedExpression<T, C> c, TypedExpression<T, D> d, TypedExpression<T, E> e, TypedExpression<T, F> f, TypedExpression<T, G> g, TypedExpression<T, H> h, TypedExpression<T, I> i, TypedExpression<T, J> j);

    <R> SelectWhereStep<T, R> selectDistinct(Class<R> projectionType);

    RowsSelectWhereStep<T, Tuple> selectDistinct(List<? extends TypedExpression<T, ?>> paths);

    <R> RowsSelectWhereStep<T, R> selectDistinct(TypedExpression<T, R> expression);

    <R> RowsSelectWhereStep<T, R> selectDistinct(Path<T, ? extends R> path);

    RowsSelectWhereStep<T, Tuple> selectDistinct(Collection<Path<T, ?>> paths);

    <A, B> RowsSelectWhereStep<T, Tuple2<A, B>> selectDistinct(Path<T, A> a, Path<T, B> b);

    <A, B, C> RowsSelectWhereStep<T, Tuple3<A, B, C>> selectDistinct(Path<T, A> a, Path<T, B> b, Path<T, C> c);

    <A, B, C, D> RowsSelectWhereStep<T, Tuple4<A, B, C, D>> selectDistinct(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d);

    <A, B, C, D, E> RowsSelectWhereStep<T, Tuple5<A, B, C, D, E>> selectDistinct(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e);

    <A, B, C, D, E, F> RowsSelectWhereStep<T, Tuple6<A, B, C, D, E, F>> selectDistinct(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f);

    <A, B, C, D, E, F, G> RowsSelectWhereStep<T, Tuple7<A, B, C, D, E, F, G>> selectDistinct(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f, Path<T, G> g);

    <A, B, C, D, E, F, G, H> RowsSelectWhereStep<T, Tuple8<A, B, C, D, E, F, G, H>> selectDistinct(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f, Path<T, G> g, Path<T, H> h);

    <A, B, C, D, E, F, G, H, I> RowsSelectWhereStep<T, Tuple9<A, B, C, D, E, F, G, H, I>> selectDistinct(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f, Path<T, G> g, Path<T, H> h, Path<T, I> i);

    <A, B, C, D, E, F, G, H, I, J> RowsSelectWhereStep<T, Tuple10<A, B, C, D, E, F, G, H, I, J>> selectDistinct(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f, Path<T, G> g, Path<T, H> h, Path<T, I> i, Path<T, J> j);

    <A, B> RowsSelectWhereStep<T, Tuple2<A, B>> selectDistinct(TypedExpression<T, A> a, TypedExpression<T, B> b);

    <A, B, C> RowsSelectWhereStep<T, Tuple3<A, B, C>> selectDistinct(TypedExpression<T, A> a, TypedExpression<T, B> b, TypedExpression<T, C> c);

    <A, B, C, D> RowsSelectWhereStep<T, Tuple4<A, B, C, D>> selectDistinct(TypedExpression<T, A> a, TypedExpression<T, B> b, TypedExpression<T, C> c, TypedExpression<T, D> d);

    <A, B, C, D, E> RowsSelectWhereStep<T, Tuple5<A, B, C, D, E>> selectDistinct(TypedExpression<T, A> a, TypedExpression<T, B> b, TypedExpression<T, C> c, TypedExpression<T, D> d, TypedExpression<T, E> e);

    <A, B, C, D, E, F> RowsSelectWhereStep<T, Tuple6<A, B, C, D, E, F>> selectDistinct(TypedExpression<T, A> a, TypedExpression<T, B> b, TypedExpression<T, C> c, TypedExpression<T, D> d, TypedExpression<T, E> e, TypedExpression<T, F> f);

    <A, B, C, D, E, F, G> RowsSelectWhereStep<T, Tuple7<A, B, C, D, E, F, G>> selectDistinct(TypedExpression<T, A> a, TypedExpression<T, B> b, TypedExpression<T, C> c, TypedExpression<T, D> d, TypedExpression<T, E> e, TypedExpression<T, F> f, TypedExpression<T, G> g);

    <A, B, C, D, E, F, G, H> RowsSelectWhereStep<T, Tuple8<A, B, C, D, E, F, G, H>> selectDistinct(TypedExpression<T, A> a, TypedExpression<T, B> b, TypedExpression<T, C> c, TypedExpression<T, D> d, TypedExpression<T, E> e, TypedExpression<T, F> f, TypedExpression<T, G> g, TypedExpression<T, H> h);

    <A, B, C, D, E, F, G, H, I> RowsSelectWhereStep<T, Tuple9<A, B, C, D, E, F, G, H, I>> selectDistinct(TypedExpression<T, A> a, TypedExpression<T, B> b, TypedExpression<T, C> c, TypedExpression<T, D> d, TypedExpression<T, E> e, TypedExpression<T, F> f, TypedExpression<T, G> g, TypedExpression<T, H> h, TypedExpression<T, I> i);

    <A, B, C, D, E, F, G, H, I, J> RowsSelectWhereStep<T, Tuple10<A, B, C, D, E, F, G, H, I, J>> selectDistinct(TypedExpression<T, A> a, TypedExpression<T, B> b, TypedExpression<T, C> c, TypedExpression<T, D> d, TypedExpression<T, E> e, TypedExpression<T, F> f, TypedExpression<T, G> g, TypedExpression<T, H> h, TypedExpression<T, I> i, TypedExpression<T, J> j);

}
