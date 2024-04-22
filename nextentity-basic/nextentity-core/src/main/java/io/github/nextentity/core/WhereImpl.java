package io.github.nextentity.core;

import io.github.nextentity.core.BasicExpressions.QueryStructureImpl;
import io.github.nextentity.core.api.EntityRoot;
import io.github.nextentity.core.api.Expression;
import io.github.nextentity.core.api.Expression.OperatableExpression;
import io.github.nextentity.core.api.ExpressionBuilder.NumberOperator;
import io.github.nextentity.core.api.ExpressionBuilder.PathOperator;
import io.github.nextentity.core.api.ExpressionBuilder.StringOperator;
import io.github.nextentity.core.api.LockModeType;
import io.github.nextentity.core.api.Operator;
import io.github.nextentity.core.api.Order;
import io.github.nextentity.core.api.Path;
import io.github.nextentity.core.api.Path.NumberPath;
import io.github.nextentity.core.api.Path.StringPath;
import io.github.nextentity.core.api.Query.Collector;
import io.github.nextentity.core.api.Query.ExpressionsBuilder;
import io.github.nextentity.core.api.Query.Having;
import io.github.nextentity.core.api.Query.OrderBy;
import io.github.nextentity.core.api.Query.OrderOperator;
import io.github.nextentity.core.api.Query.SubQueryBuilder;
import io.github.nextentity.core.api.Query.Where0;
import io.github.nextentity.core.api.expression.BaseExpression;
import io.github.nextentity.core.api.expression.Operation;
import io.github.nextentity.core.api.expression.QueryStructure;
import io.github.nextentity.core.api.expression.QueryStructure.Selected;
import io.github.nextentity.core.api.expression.QueryStructure.Selected.SelectArray;
import io.github.nextentity.core.api.expression.QueryStructure.Selected.SelectPrimitive;
import io.github.nextentity.core.util.Lists;
import io.github.nextentity.core.util.Paths;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.List;
import java.util.function.Function;

@SuppressWarnings("PatternVariableCanBeUsed")
public class WhereImpl<T, U> implements Where0<T, U>, Having<T, U>, AbstractCollector<U> {

    static final Selected SELECT_ANY = new SelectPrimitive().expression(BasicExpressions.TRUE).type(Boolean.class);
    static final Selected COUNT_ANY = new SelectPrimitive()
            .expression(BasicExpressions.operate(BasicExpressions.TRUE, Operator.COUNT)).type(Long.class);

    QueryExecutor queryExecutor;
    QueryStructureImpl queryStructure;
    QueryPostProcessor structurePostProcessor;

    public WhereImpl(QueryExecutor queryExecutor, Class<T> type, QueryPostProcessor structurePostProcessor) {
        init(type, queryExecutor, structurePostProcessor);
    }


    WhereImpl(QueryExecutor queryExecutor, QueryStructureImpl queryStructure, QueryPostProcessor structurePostProcessor) {
        init(queryExecutor, queryStructure, structurePostProcessor);
    }

    protected void init(Class<T> type, QueryExecutor queryExecutor, QueryPostProcessor structurePostProcessor) {
        init(queryExecutor, new QueryStructureImpl(queryExecutor.metamodel().getEntity(type)), structurePostProcessor);
    }

    private void init(QueryExecutor queryExecutor, QueryStructureImpl queryStructure, QueryPostProcessor structurePostProcessor) {
        this.queryExecutor = queryExecutor;
        this.queryStructure = queryStructure;
        this.structurePostProcessor = structurePostProcessor == null ? QueryPostProcessor.NONE : structurePostProcessor;
    }

    public WhereImpl() {
    }

    <X, Y> WhereImpl<X, Y> update(QueryStructureImpl queryStructure) {
        return new WhereImpl<>(queryExecutor, queryStructure, structurePostProcessor);
    }

    @Override
    public Where0<T, U> where(Expression<T, Boolean> predicate) {
        if (BasicExpressions.isNullOrTrue(predicate)) {
            return this;
        }
        QueryStructureImpl structure = queryStructure.copy();
        whereAnd(structure, predicate);
        return update(structure);
    }

    static void whereAnd(QueryStructureImpl structure, BaseExpression expression) {
        if (BasicExpressions.isNullOrTrue(structure.where)) {
            structure.where = expression;
        } else {
            structure.where = BasicExpressions.operate(structure.where, Operator.AND, expression);
        }
    }

    @Override
    public Collector<U> orderBy(List<? extends Order<T>> orders) {
        return addOrderBy(orders);
    }

    @Override
    public Collector<U> orderBy(Function<EntityRoot<T>, List<? extends Order<T>>> ordersBuilder) {
        return orderBy(ordersBuilder.apply(Paths.root()));
    }

    @Override
    public OrderOperator<T, U> orderBy(Collection<Path<T, Comparable<?>>> paths) {
        return new OrderOperatorImpl<>(this, paths);
    }

    WhereImpl<T, U> addOrderBy(List<? extends Order<T>> orders) {
        QueryStructureImpl structure = queryStructure.copy();
        structure.orderBy = structure.orderBy == null ? orders : Lists.concat(structure.orderBy, orders);
        return update(structure);
    }

    @Override
    public long count() {
        QueryStructure structure = buildCountData();
        structure = structurePostProcessor.preCountQuery(this, structure);
        return queryExecutor.<Number>getList(structure).get(0).longValue();
    }

    @NotNull
    BasicExpressions.QueryStructureImpl buildCountData() {
        QueryStructureImpl structure = queryStructure.copy();
        structure.lockType = LockModeType.NONE;
        structure.orderBy = Lists.of();
        if (queryStructure.select().distinct()) {
            return new QueryStructureImpl(COUNT_ANY, structure);
        } else if (requiredCountSubQuery(queryStructure.select())) {
            structure.select = COUNT_ANY;
            return new QueryStructureImpl(COUNT_ANY, structure);
        } else if (queryStructure.groupBy() != null && !queryStructure.groupBy().isEmpty()) {
            structure.select = SELECT_ANY;
            return new QueryStructureImpl(COUNT_ANY, structure);
        } else {
            structure.select = COUNT_ANY;
            return structure;
        }
    }

    boolean requiredCountSubQuery(Selected select) {
        if (select instanceof SelectPrimitive) {
            return requiredCountSubQuery(((SelectPrimitive) select).expression());
        }
        if (select instanceof SelectArray) {
            for (Selected expression : ((SelectArray) select).items()) {
                if (requiredCountSubQuery(expression)) {
                    return true;
                }
            }
        }
        return false;
    }

    protected boolean requiredCountSubQuery(BaseExpression expression) {
        if (expression instanceof Operation) {
            Operation operation = (Operation) expression;
            if (operation.operator().isAgg()) {
                return true;
            }
            List<? extends BaseExpression> args = operation.operands();
            if (args != null) {
                for (BaseExpression arg : args) {
                    if (requiredCountSubQuery(arg)) {
                        return true;
                    }
                }
            }
        }
        return false;
    }

    @Override
    public List<U> getList(int offset, int maxResult, LockModeType lockModeType) {
        QueryStructure structure = buildListData(offset, maxResult, lockModeType);
        structure = structurePostProcessor.preListQuery(this, structure);
        return queryList(structure);
    }

    public <X> List<X> queryList(QueryStructure structure) {
        return queryExecutor.getList(structure);
    }

    @NotNull
    BasicExpressions.QueryStructureImpl buildListData(int offset, int maxResult, LockModeType lockModeType) {
        QueryStructureImpl structure = queryStructure.copy();
        structure.offset = offset;
        structure.limit = maxResult;
        structure.lockType = lockModeType;
        return structure;
    }

    @Override
    public boolean exist(int offset) {
        QueryStructure structure = buildExistData(offset);
        structure = structurePostProcessor.preExistQuery(this, structure);
        return !queryList(structure).isEmpty();
    }

    @NotNull
    BasicExpressions.QueryStructureImpl buildExistData(int offset) {
        QueryStructureImpl structure = queryStructure.copy();
        structure.select = SELECT_ANY;
        structure.offset = offset;
        structure.limit = 1;
        structure.orderBy = Lists.of();
        return structure;
    }

    @Override
    public <X> SubQueryBuilder<X, U> asSubQuery() {
        return new SubQuery<>();
    }

    @Override
    public Having<T, U> groupBy(List<? extends Expression<T, ?>> expressions) {
        QueryStructureImpl structure = queryStructure.copy();
        structure.groupBy = expressions;
        return update(structure);
    }

    @Override
    public Having<T, U> groupBy(ExpressionsBuilder<T> expressionsBuilder) {
        return groupBy(expressionsBuilder.apply(Paths.root()));
    }

    @Override
    public Having<T, U> groupBy(Path<T, ?> path) {
        QueryStructureImpl structure = queryStructure.copy();
        structure.groupBy = Lists.of(BasicExpressions.of(path));
        return update(structure);
    }

    @Override
    public Having<T, U> groupBy(Collection<Path<T, ?>> paths) {
        return groupBy(BasicExpressions.toExpressionList(paths));
    }

    @Override
    public OrderBy<T, U> having(Expression<T, Boolean> predicate) {
        QueryStructureImpl structure = queryStructure.copy();
        structure.having = predicate;
        return update(structure);
    }

    @Override
    public <N extends Number> NumberOperator<T, N, Where0<T, U>> where(NumberPath<T, N> path) {
        return ExpressionBuilders.ofNumber(root().get(path), this::whereAnd);
    }

    @NotNull
    private Where0<T, U> whereAnd(OperatableExpression<?, ?> expression) {
        if (expression == null) {
            return this;
        }
        QueryStructureImpl structure = queryStructure.copy();
        whereAnd(structure, expression);
        return update(structure);
    }

    @Override
    public StringOperator<T, Where0<T, U>> where(StringPath<T> path) {
        return ExpressionBuilders.ofString(root().get(path), this::whereAnd);
    }

    public EntityRoot<T> root() {
        return Paths.root();
    }

    @Override
    public <N> PathOperator<T, N, Where0<T, U>> where(Path<T, N> path) {
        return ExpressionBuilders.ofPath(root().get(path), this::whereAnd);
    }


    class SubQuery<X> implements SubQueryBuilder<X, U>, QueryStructure {

        @Override
        public Expression<X, Long> count() {
            QueryStructure structure = buildCountData();
            return Expressions.of(structure);
        }

        @Override
        public Expression<X, List<U>> slice(int offset, int maxResult) {
            QueryStructure structure = buildListData(offset, maxResult, null);
            return Expressions.of(structure);
        }

        @Override
        public Expression<X, U> getSingle(int offset) {
            QueryStructure structure = buildListData(offset, 2, null);
            return Expressions.of(structure);
        }

        @Override
        public Expression<X, U> getFirst(int offset) {
            QueryStructure structure = buildListData(offset, 1, null);
            return Expressions.of(structure);
        }

        public LockModeType lockType() {
            return queryStructure.lockType();
        }

        public Integer limit() {
            return queryStructure.limit();
        }

        public Integer offset() {
            return queryStructure.offset();
        }

        public BaseExpression having() {
            return queryStructure.having();
        }

        public List<? extends Order<?>> orderBy() {
            return queryStructure.orderBy();
        }

        public List<? extends BaseExpression> groupBy() {
            return queryStructure.groupBy();
        }

        public BaseExpression where() {
            return queryStructure.where();
        }

        public From from() {
            return queryStructure.from();
        }

        public Selected select() {
            return queryStructure.select();
        }
    }

}
