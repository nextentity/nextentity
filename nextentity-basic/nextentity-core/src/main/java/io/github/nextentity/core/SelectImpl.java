package io.github.nextentity.core;

import io.github.nextentity.core.ExpressionTrees.QueryStructureImpl;
import io.github.nextentity.core.api.Expression;
import io.github.nextentity.core.api.Expression.PathExpression;
import io.github.nextentity.core.api.ExpressionTree;
import io.github.nextentity.core.api.ExpressionTree.ExpressionNode;
import io.github.nextentity.core.api.Path;
import io.github.nextentity.core.api.Query.ExpressionsBuilder;
import io.github.nextentity.core.api.Query.Fetch;
import io.github.nextentity.core.api.Query.Select;
import io.github.nextentity.core.api.Query.Where0;
import io.github.nextentity.core.expression.Attribute;
import io.github.nextentity.core.expression.MultiSelected;
import io.github.nextentity.core.expression.SelectElement;
import io.github.nextentity.core.expression.SelectEntity;
import io.github.nextentity.core.expression.SelectExpression;
import io.github.nextentity.core.expression.SingleSelected;
import io.github.nextentity.core.meta.graph.EntityReferenced;
import io.github.nextentity.core.meta.graph.EntitySchema;
import io.github.nextentity.core.meta.graph.ProjectionSchema;
import io.github.nextentity.core.util.Lists;
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

import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;


@SuppressWarnings("PatternVariableCanBeUsed")
public class SelectImpl<T> extends WhereImpl<T, T> implements Select<T>, Fetch<T> {

    SelectImpl() {
    }

    public SelectImpl(QueryExecutor queryExecutor, Class<T> type, QueryPostProcessor structurePostProcessor) {
        super(queryExecutor, type, structurePostProcessor);
    }

    public Where0<T, T> fetch(List<PathExpression<T, ?>> expressions) {
        QueryStructureImpl structure = queryStructure.copy();
        EntitySchema entity = queryExecutor.metamodel().getEntity(queryStructure.from.type());
        Set<EntityReferenced> fetchAttributes = new HashSet<>(expressions.size() << 1);
        for (PathExpression<T, ?> expression : expressions) {
            ExpressionNode expr = expression.rootNode();
            if (expr instanceof Attribute) {
                Attribute column = (Attribute) expr;
                EntityReferenced attribute = (EntityReferenced) column.toAttribute(entity);
                fetchAttributes.add(attribute);
            }
        }
        SingleSelected select = (SingleSelected) structure.select;
        SelectEntity element = (SelectEntity) select.element();
        element = element.fetch(fetchAttributes);
        structure.select = new SingleSelected(element, structure.select.distinct());
        return update(structure);
    }

    @Override
    public Where0<T, Tuple> select(ExpressionsBuilder<T> selectBuilder) {
        return select(selectBuilder.apply(io.github.nextentity.core.util.Paths.root()));
    }

    @Override
    public <R> Where0<T, R> selectDistinct(Class<R> projectionType) {
        return select(true, projectionType);
    }

    @Override
    public <R> Where0<T, R> select(Class<R> projectionType) {
        return select(false, projectionType);
    }

    public <R> Where0<T, R> select(boolean distinct, Class<R> projectionType) {
        Class<?> fromType = queryStructure.from.type();
        if (projectionType == fromType) {
            return update(queryStructure);
        }
        QueryStructureImpl structure = queryStructure.copy();
        ProjectionSchema projection = queryExecutor.metamodel().getProjection(fromType, projectionType);
        SelectEntity select = SelectEntity.of(projection);
        structure.select = new SingleSelected(select, distinct);
        return update(structure);
    }

    public <R> Where0<T, R> selectDistinct(Path<T, ? extends R> path) {
        return select(true, path);
    }

    public <R> Where0<T, R> select(Path<T, ? extends R> path) {
        return select(false, path);
    }

    public <R> Where0<T, R> select(boolean distinct, Path<T, ? extends R> path) {
        QueryStructureImpl structure = queryStructure.copy();
        SelectElement element = toSelectElement(path);
        structure.select = new SingleSelected(element, distinct);
        return update(structure);
    }

    private <R> @NotNull SelectElement toSelectElement(Path<T, ? extends R> path) {
        Attribute paths = ExpressionTrees.of(path);
        return selectElement(paths);
    }

    public Where0<T, Tuple> selectDistinct(Collection<Path<T, ?>> paths) {
        return selectDistinct(ExpressionTrees.toExpressionList(paths));
    }

    public Where0<T, Tuple> select(Collection<Path<T, ?>> paths) {
        return select(ExpressionTrees.toExpressionList(paths));
    }

    @Override
    public <A, B> Where0<T, Tuple2<A, B>> select(Path<T, A> a, Path<T, B> b) {
        return selectTuple(false, Lists.of(a, b));
    }

    @Override
    public <A, B, C> Where0<T, Tuple3<A, B, C>> select(Path<T, A> a, Path<T, B> b, Path<T, C> c) {
        return selectTuple(false, Lists.of(a, b, c));
    }

    @Override
    public <A, B, C, D> Where0<T, Tuple4<A, B, C, D>> select(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d) {
        return selectTuple(false, Lists.of(a, b, c, d));
    }

    @Override
    public <A, B, C, D, E> Where0<T, Tuple5<A, B, C, D, E>> select(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e) {
        return selectTuple(false, Lists.of(a, b, c, d, e));
    }

    @Override
    public <A, B, C, D, E, F> Where0<T, Tuple6<A, B, C, D, E, F>> select(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f) {
        return selectTuple(false, Lists.of(a, b, c, d, e, f));
    }

    @Override
    public <A, B, C, D, E, F, G> Where0<T, Tuple7<A, B, C, D, E, F, G>> select(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f, Path<T, G> g) {
        return selectTuple(false, Lists.of(a, b, c, d, e, f, g));
    }

    @Override
    public <A, B, C, D, E, F, G, H> Where0<T, Tuple8<A, B, C, D, E, F, G, H>> select(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f, Path<T, G> g, Path<T, H> h) {
        return selectTuple(false, Lists.of(a, b, c, d, e, f, g, h));
    }

    @Override
    public <A, B, C, D, E, F, G, H, I> Where0<T, Tuple9<A, B, C, D, E, F, G, H, I>> select(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f, Path<T, G> g, Path<T, H> h, Path<T, I> i) {
        return selectTuple(false, Lists.of(a, b, c, d, e, f, g, h, i));
    }

    @Override
    public <A, B, C, D, E, F, G, H, I, J> Where0<T, Tuple10<A, B, C, D, E, F, G, H, I, J>> select(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f, Path<T, G> g, Path<T, H> h, Path<T, I> i, Path<T, J> j) {
        return selectTuple(false, Lists.of(a, b, c, d, e, f, g, h, i, j));
    }

    public Where0<T, Tuple> selectDistinct(List<? extends Expression<T, ?>> expressions) {
        return select(true, expressions);
    }

    @Override
    public Where0<T, Tuple> selectDistinct(ExpressionsBuilder<T> selectBuilder) {
        return selectDistinct(selectBuilder.apply(io.github.nextentity.core.util.Paths.root()));
    }

    @Override
    public <A, B> Where0<T, Tuple2<A, B>> selectDistinct(Path<T, A> a, Path<T, B> b) {
        return selectTuple(true, Lists.of(a, b));
    }

    @Override
    public <A, B, C> Where0<T, Tuple3<A, B, C>> selectDistinct(Path<T, A> a, Path<T, B> b, Path<T, C> c) {
        return selectTuple(true, Lists.of(a, b, c));
    }

    @Override
    public <A, B, C, D> Where0<T, Tuple4<A, B, C, D>> selectDistinct(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d) {
        return selectTuple(true, Lists.of(a, b, c, d));
    }

    @Override
    public <A, B, C, D, E> Where0<T, Tuple5<A, B, C, D, E>> selectDistinct(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e) {
        return selectTuple(true, Lists.of(a, b, c, d, e));
    }

    @Override
    public <A, B, C, D, E, F> Where0<T, Tuple6<A, B, C, D, E, F>> selectDistinct(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f) {
        return selectTuple(true, Lists.of(a, b, c, d, e, f));
    }

    @Override
    public <A, B, C, D, E, F, G> Where0<T, Tuple7<A, B, C, D, E, F, G>> selectDistinct(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f, Path<T, G> g) {
        return selectTuple(true, Lists.of(a, b, c, d, e, f, g));
    }

    @Override
    public <A, B, C, D, E, F, G, H> Where0<T, Tuple8<A, B, C, D, E, F, G, H>> selectDistinct(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f, Path<T, G> g, Path<T, H> h) {
        return selectTuple(true, Lists.of(a, b, c, d, e, f, g, h));
    }

    @Override
    public <A, B, C, D, E, F, G, H, I> Where0<T, Tuple9<A, B, C, D, E, F, G, H, I>> selectDistinct(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f, Path<T, G> g, Path<T, H> h, Path<T, I> i) {
        return selectTuple(true, Lists.of(a, b, c, d, e, f, g, h, i));
    }

    @Override
    public <A, B, C, D, E, F, G, H, I, J> Where0<T, Tuple10<A, B, C, D, E, F, G, H, I, J>> selectDistinct(Path<T, A> a, Path<T, B> b, Path<T, C> c, Path<T, D> d, Path<T, E> e, Path<T, F> f, Path<T, G> g, Path<T, H> h, Path<T, I> i, Path<T, J> j) {
        return selectTuple(true, Lists.of(a, b, c, d, e, f, g, h, i, j));
    }

    public Where0<T, Tuple> select(List<? extends Expression<T, ?>> expressions) {
        return select(false, expressions);
    }

    public Where0<T, Tuple> select(boolean distinct, List<? extends Expression<T, ?>> expressions) {
        QueryStructureImpl structure = queryStructure.copy();
        List<SelectElement> selectExpressions = expressions.stream()
                .map(this::selectElement)
                .collect(Collectors.toList());
        structure.select = new MultiSelected(selectExpressions, distinct);
        return update(structure);
    }

    public <R extends Tuple> Where0<T, R> selectTuple(boolean distinct, List<? extends Path<T, ?>> paths) {
        QueryStructureImpl structure = queryStructure.copy();
        List<SelectElement> selectExpressions = paths.stream()
                .map(this::toSelectElement)
                .collect(Collectors.toList());
        structure.select = new MultiSelected(selectExpressions, distinct);
        return update(structure);
    }


    private SelectElement selectElement(ExpressionTree expression) {
        if (expression.rootNode() instanceof Attribute) {
            expression = ((Attribute) expression.rootNode()).toAttribute(queryExecutor.metamodel().getEntity(queryStructure.from().type()));
        }
        if (expression instanceof EntityReferenced) {
            return SelectEntity.of((EntityReferenced) expression);
        }
        return SelectExpression.of(expression);
    }

    @Override
    public <A, B> Where0<T, Tuple2<A, B>> select(Expression<T, A> a, Expression<T, B> b) {
        return selectTupleByExpr(false, a, b);
    }

    @Override
    public <A, B, C> Where0<T, Tuple3<A, B, C>> select(Expression<T, A> a, Expression<T, B> b, Expression<T, C> c) {
        return selectTupleByExpr(false, a, b, c);
    }

    @Override
    public <A, B, C, D> Where0<T, Tuple4<A, B, C, D>> select(Expression<T, A> a, Expression<T, B> b, Expression<T, C> c, Expression<T, D> d) {
        return selectTupleByExpr(false, a, b, c, d);
    }

    @Override
    public <A, B, C, D, E> Where0<T, Tuple5<A, B, C, D, E>> select(Expression<T, A> a, Expression<T, B> b, Expression<T, C> c, Expression<T, D> d, Expression<T, E> e) {
        return selectTupleByExpr(false, a, b, c, d, e);
    }

    @Override
    public <A, B, C, D, E, F> Where0<T, Tuple6<A, B, C, D, E, F>> select(Expression<T, A> a, Expression<T, B> b, Expression<T, C> c, Expression<T, D> d, Expression<T, E> e, Expression<T, F> f) {
        return selectTupleByExpr(false, a, b, c, d, e, f);
    }

    @Override
    public <A, B, C, D, E, F, G> Where0<T, Tuple7<A, B, C, D, E, F, G>> select(Expression<T, A> a, Expression<T, B> b, Expression<T, C> c, Expression<T, D> d, Expression<T, E> e, Expression<T, F> f, Expression<T, G> g) {
        return selectTupleByExpr(false, a, b, c, d, e, f, g);
    }

    @Override
    public <A, B, C, D, E, F, G, H> Where0<T, Tuple8<A, B, C, D, E, F, G, H>> select(Expression<T, A> a, Expression<T, B> b, Expression<T, C> c, Expression<T, D> d, Expression<T, E> e, Expression<T, F> f, Expression<T, G> g, Expression<T, H> h) {
        return selectTupleByExpr(false, a, b, c, d, e, f, g, h);
    }

    @Override
    public <A, B, C, D, E, F, G, H, I> Where0<T, Tuple9<A, B, C, D, E, F, G, H, I>> select(Expression<T, A> a, Expression<T, B> b, Expression<T, C> c, Expression<T, D> d, Expression<T, E> e, Expression<T, F> f, Expression<T, G> g, Expression<T, H> h, Expression<T, I> i) {
        return selectTupleByExpr(false, a, b, c, d, e, f, g, h, i);
    }

    @Override
    public <A, B, C, D, E, F, G, H, I, J> Where0<T, Tuple10<A, B, C, D, E, F, G, H, I, J>> select(Expression<T, A> a, Expression<T, B> b, Expression<T, C> c, Expression<T, D> d, Expression<T, E> e, Expression<T, F> f, Expression<T, G> g, Expression<T, H> h, Expression<T, I> i, Expression<T, J> j) {
        return selectTupleByExpr(false, a, b, c, d, e, f, g, h, i, j);
    }

    @Override
    public <A, B> Where0<T, Tuple2<A, B>> selectDistinct(Expression<T, A> a, Expression<T, B> b) {
        return selectTupleByExpr(true, a, b);
    }

    @Override
    public <A, B, C> Where0<T, Tuple3<A, B, C>> selectDistinct(Expression<T, A> a, Expression<T, B> b, Expression<T, C> c) {
        return selectTupleByExpr(true, a, b, c);
    }

    @Override
    public <A, B, C, D> Where0<T, Tuple4<A, B, C, D>> selectDistinct(Expression<T, A> a, Expression<T, B> b, Expression<T, C> c, Expression<T, D> d) {
        return selectTupleByExpr(true, a, b, c, d);
    }

    @Override
    public <A, B, C, D, E> Where0<T, Tuple5<A, B, C, D, E>> selectDistinct(Expression<T, A> a, Expression<T, B> b, Expression<T, C> c, Expression<T, D> d, Expression<T, E> e) {
        return selectTupleByExpr(true, a, b, c, d, e);
    }

    @Override
    public <A, B, C, D, E, F> Where0<T, Tuple6<A, B, C, D, E, F>> selectDistinct(Expression<T, A> a, Expression<T, B> b, Expression<T, C> c, Expression<T, D> d, Expression<T, E> e, Expression<T, F> f) {
        return selectTupleByExpr(true, a, b, c, d, e, f);
    }

    @Override
    public <A, B, C, D, E, F, G> Where0<T, Tuple7<A, B, C, D, E, F, G>> selectDistinct(Expression<T, A> a, Expression<T, B> b, Expression<T, C> c, Expression<T, D> d, Expression<T, E> e, Expression<T, F> f, Expression<T, G> g) {
        return selectTupleByExpr(true, a, b, c, d, e, f, g);
    }

    @Override
    public <A, B, C, D, E, F, G, H> Where0<T, Tuple8<A, B, C, D, E, F, G, H>> selectDistinct(Expression<T, A> a, Expression<T, B> b, Expression<T, C> c, Expression<T, D> d, Expression<T, E> e, Expression<T, F> f, Expression<T, G> g, Expression<T, H> h) {
        return selectTupleByExpr(true, a, b, c, d, e, f, g, h);
    }

    @Override
    public <A, B, C, D, E, F, G, H, I> Where0<T, Tuple9<A, B, C, D, E, F, G, H, I>> selectDistinct(Expression<T, A> a, Expression<T, B> b, Expression<T, C> c, Expression<T, D> d, Expression<T, E> e, Expression<T, F> f, Expression<T, G> g, Expression<T, H> h, Expression<T, I> i) {
        return selectTupleByExpr(true, a, b, c, d, e, f, g, h, i);
    }

    @Override
    public <A, B, C, D, E, F, G, H, I, J> Where0<T, Tuple10<A, B, C, D, E, F, G, H, I, J>> selectDistinct(Expression<T, A> a, Expression<T, B> b, Expression<T, C> c, Expression<T, D> d, Expression<T, E> e, Expression<T, F> f, Expression<T, G> g, Expression<T, H> h, Expression<T, I> i, Expression<T, J> j) {
        return selectTupleByExpr(true, a, b, c, d, e, f, g, h, i, j);
    }


    @SafeVarargs
    public final <R extends Tuple> Where0<T, R> selectTupleByExpr(boolean distinct, Expression<T, ?>... expressions) {
        QueryStructureImpl structure = queryStructure.copy();
        List<SelectElement> selectExpressions = Arrays.stream(expressions)
                .map(this::selectElement)
                .collect(Collectors.toList());
        structure.select = new MultiSelected(selectExpressions, distinct);
        return update(structure);
    }

    public <R> Where0<T, R> selectDistinct(Expression<T, R> paths) {
        return select(true, paths);
    }

    public <R> Where0<T, R> select(Expression<T, R> paths) {
        return select(false, paths);
    }

    public <R> Where0<T, R> select(boolean distinct, Expression<T, R> expression) {
        QueryStructureImpl structure = queryStructure.copy();
        SelectElement node = selectElement(expression);
        structure.select = new SingleSelected(node, distinct);
        return update(structure);
    }

    protected Class<?> getType(Path<?, ?> path) {
        return PathReference.of(path).getReturnType();
    }

    @Override
    public String toString() {
        return "QueryBuilder[" + queryExecutor.getClass().getSimpleName() + "]";
    }
}
