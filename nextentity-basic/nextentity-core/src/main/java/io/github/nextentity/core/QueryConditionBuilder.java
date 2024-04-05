package io.github.nextentity.core;

import io.github.nextentity.core.ExpressionTrees.QueryStructureImpl;
import io.github.nextentity.core.ExpressionTrees.SingleSelectedImpl;
import io.github.nextentity.core.api.Expression;
import io.github.nextentity.core.api.Expression.ExpressionTree;
import io.github.nextentity.core.api.Expression.Operation;
import io.github.nextentity.core.api.Expression.QueryStructure;
import io.github.nextentity.core.api.ExpressionOperator.NumberOperator;
import io.github.nextentity.core.api.ExpressionOperator.PathOperator;
import io.github.nextentity.core.api.ExpressionOperator.StringOperator;
import io.github.nextentity.core.api.Lists;
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
import io.github.nextentity.core.api.Query.QueryStructureBuilder;
import io.github.nextentity.core.api.Query.SliceQueryStructure;
import io.github.nextentity.core.api.Query.SubQueryBuilder;
import io.github.nextentity.core.api.Query.Where0;
import io.github.nextentity.core.api.Root;
import io.github.nextentity.core.api.Selection;
import io.github.nextentity.core.api.Selection.MultiSelected;
import io.github.nextentity.core.api.TypedExpression;
import io.github.nextentity.core.api.TypedExpression.BasicExpression;
import io.github.nextentity.core.util.Paths;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

@SuppressWarnings("PatternVariableCanBeUsed")
public class QueryConditionBuilder<T, U> implements Where0<T, U>, Having<T, U>, AbstractCollector<U> {

    static final SingleSelectedImpl SELECT_ANY = new SingleSelectedImpl(Integer.class, Expressions.TRUE, false);

    static final SingleSelectedImpl COUNT_ANY = new SingleSelectedImpl(Integer.class, Expressions.operate(Expressions.TRUE, Operator.COUNT), false);

    final QueryExecutor queryExecutor;
    final QueryStructureImpl queryStructure;

    protected final QueryStructurePostProcessor structurePostProcessor;

    public QueryConditionBuilder(QueryExecutor queryExecutor, Class<T> type, QueryStructurePostProcessor structurePostProcessor) {
        this(queryExecutor, new QueryStructureImpl(type), structurePostProcessor);
    }

    QueryConditionBuilder(QueryExecutor queryExecutor, QueryStructureImpl queryStructure, QueryStructurePostProcessor structurePostProcessor) {
        this.queryExecutor = queryExecutor;
        this.queryStructure = queryStructure;
        this.structurePostProcessor = structurePostProcessor == null ? QueryStructurePostProcessor.NONE : structurePostProcessor;
    }

    <X, Y> QueryConditionBuilder<X, Y> update(QueryStructureImpl queryStructure) {
        return new QueryConditionBuilder<>(queryExecutor, queryStructure, structurePostProcessor);
    }

    @Override
    public Where0<T, U> where(TypedExpression<T, Boolean> predicate) {
        Expression expression = predicate.tree();
        if (Expressions.isNullOrTrue(expression)) {
            return this;
        }
        QueryStructureImpl structure = queryStructure.copy();
        whereAnd(structure, expression);
        return update(structure);
    }

    static void whereAnd(QueryStructureImpl structure, Expression expression) {
        if (Expressions.isNullOrTrue(structure.where)) {
            structure.where = expression.tree();
        } else {
            structure.where = Expressions.operate(structure.where, Operator.AND, expression);
        }
    }

    @Override
    public Collector<U> orderBy(List<? extends Order<T>> orders) {
        return addOrderBy(orders);
    }

    @Override
    public Collector<U> orderBy(Function<Root<T>, List<? extends Order<T>>> ordersBuilder) {
        return orderBy(ordersBuilder.apply(Paths.root()));
    }

    @Override
    public OrderOperator<T, U> orderBy(Collection<Path<T, Comparable<?>>> paths) {
        return new OrderOperatorImpl<>(this, paths);
    }

    QueryConditionBuilder<T, U> addOrderBy(List<? extends Order<T>> orders) {
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

    @NotNull ExpressionTrees.QueryStructureImpl buildCountData() {
        QueryStructureImpl structure = queryStructure.copy();
        structure.lockType = LockModeType.NONE;
        structure.orderBy = Lists.of();
        if (queryStructure.select().distinct()) {
            return new QueryStructureImpl(COUNT_ANY, structure);
        } else if (requiredCountSubQuery(queryStructure)) {
            structure.select = COUNT_ANY;
            return new QueryStructureImpl(COUNT_ANY, structure);
        } else if (queryStructure.groupBy() != null && !queryStructure.groupBy().isEmpty()) {
            structure.select = SELECT_ANY;
            structure.fetch = Lists.of();
            return new QueryStructureImpl(COUNT_ANY, structure);
        } else {
            structure.select = COUNT_ANY;
            structure.fetch = Lists.of();
            return structure;
        }
    }

    boolean requiredCountSubQuery(QueryStructureImpl structure) {
        Selection select = structure.select();
        if (select instanceof SingleSelectedImpl) {
            ExpressionTree column = ((SingleSelectedImpl) select).expression();
            return requiredCountSubQuery(column);
        } else if (select instanceof MultiSelected) {
            List<? extends ExpressionTree> columns = ((MultiSelected) select).expressions();
            if (requiredCountSubQuery(columns)) {
                return true;
            }
        }
        return requiredCountSubQuery(structure.having());
    }

    protected boolean requiredCountSubQuery(List<? extends ExpressionTree> expressions) {
        for (ExpressionTree expression : expressions) {
            if (requiredCountSubQuery(expression)) {
                return true;
            }
        }
        return false;
    }

    protected boolean requiredCountSubQuery(ExpressionTree expression) {
        if (expression instanceof Operation) {
            Operation operation = (Operation) expression;
            if (operation.operator().isAgg()) {
                return true;
            }
            List<? extends ExpressionTree> args = operation.operands();
            if (args != null) {
                for (ExpressionTree arg : args) {
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

    @NotNull ExpressionTrees.QueryStructureImpl buildListData(int offset, int maxResult, LockModeType lockModeType) {
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

    @NotNull ExpressionTrees.QueryStructureImpl buildExistData(int offset) {
        QueryStructureImpl structure = queryStructure.copy();
        structure.select = SELECT_ANY;
        structure.offset = offset;
        structure.limit = 1;
        structure.fetch = Lists.of();
        structure.orderBy = Lists.of();
        return structure;
    }

    @Override
    public QueryStructureBuilder buildMetadata() {
        return new QueryStructureBuilder() {
            @Override
            public QueryStructure count() {
                return buildCountData();
            }

            @Override
            public QueryStructure getList(int offset, int maxResult, LockModeType lockModeType) {
                return buildListData(offset, maxResult, lockModeType);
            }

            @Override
            public QueryStructure exist(int offset) {
                return buildExistData(offset);
            }

            @Override
            public SliceQueryStructure slice(int offset, int limit) {
                return new SliceQueryStructure(buildCountData(), buildListData(offset, limit, LockModeType.NONE));
            }

        };
    }

    @Override
    public <X> SubQueryBuilder<X, U> asSubQuery() {
        return new SubQuery<>();
    }

    @Override
    public Having<T, U> groupBy(List<? extends TypedExpression<T, ?>> expressions) {
        QueryStructureImpl structure = queryStructure.copy();
        structure.groupBy = expressions.stream().map(TypedExpression::tree).collect(Collectors.toList());
        return update(structure);
    }

    @Override
    public Having<T, U> groupBy(ExpressionsBuilder<T> expressionsBuilder) {
        return groupBy(expressionsBuilder.apply(Paths.root()));
    }

    @Override
    public Having<T, U> groupBy(Path<T, ?> path) {
        QueryStructureImpl structure = queryStructure.copy();
        structure.groupBy = Lists.of(Expressions.of(path));
        return update(structure);
    }

    @Override
    public Having<T, U> groupBy(Collection<Path<T, ?>> paths) {
        return groupBy(Expressions.toExpressionList(paths));
    }

    @Override
    public OrderBy<T, U> having(TypedExpression<T, Boolean> predicate) {
        QueryStructureImpl structure = queryStructure.copy();
        structure.having = predicate.tree();
        return update(structure);
    }

    @Override
    public <N extends Number> NumberOperator<T, N, Where0<T, U>> where(NumberPath<T, N> path) {
        return ExpressionOperators.ofNumber(root().get(path), this::whereAnd);
    }

    @NotNull
    private Where0<T, U> whereAnd(BasicExpression<?, ?> expression) {
        if (expression == null) {
            return this;
        }
        QueryStructureImpl structure = queryStructure.copy();
        whereAnd(structure, expression.tree());
        return update(structure);
    }

    @Override
    public StringOperator<T, Where0<T, U>> where(StringPath<T> path) {
        return ExpressionOperators.ofString(root().get(path), this::whereAnd);
    }

    public Root<T> root() {
        return Paths.root();
    }

    @Override
    public <N> PathOperator<T, N, Where0<T, U>> where(Path<T, N> path) {
        return ExpressionOperators.ofPath(root().get(path), this::whereAnd);
    }


    class SubQuery<X> implements SubQueryBuilder<X, U> {

        @Override
        public TypedExpression<X, Long> count() {
            QueryStructure structure = buildCountData();
            structure = structurePostProcessor.preCountQuery(QueryConditionBuilder.this, structure);
            return TypedExpressions.of(structure);
        }

        @Override
        public TypedExpression<X, List<U>> slice(int offset, int maxResult) {
            QueryStructure structure = buildListData(offset, maxResult, null);
            structure = structurePostProcessor.preListQuery(QueryConditionBuilder.this, structure);
            return TypedExpressions.of(structure);
        }

        @Override
        public TypedExpression<X, U> getSingle(int offset) {
            QueryStructure structure = buildListData(offset, 2, null);
            structure = structurePostProcessor.preListQuery(QueryConditionBuilder.this, structure);
            return TypedExpressions.of(structure);
        }

        @Override
        public TypedExpression<X, U> getFirst(int offset) {
            QueryStructure structure = buildListData(offset, 1, null);
            structure = structurePostProcessor.preListQuery(QueryConditionBuilder.this, structure);
            return TypedExpressions.of(structure);
        }

        @Override
        public ExpressionTree tree() {
            QueryStructure structure = buildListData(-1, -1, null);
            structure = structurePostProcessor.preListQuery(QueryConditionBuilder.this, structure);
            return structure;
        }

    }

}
