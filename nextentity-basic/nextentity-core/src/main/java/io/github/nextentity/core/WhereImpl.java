package io.github.nextentity.core;

import io.github.nextentity.api.Collector;
import io.github.nextentity.api.Expression;
import io.github.nextentity.api.ExpressionBuilder.NumberOperator;
import io.github.nextentity.api.ExpressionBuilder.PathOperator;
import io.github.nextentity.api.ExpressionBuilder.StringOperator;
import io.github.nextentity.api.SelectHavingStep;
import io.github.nextentity.api.SelectOrderByStep;
import io.github.nextentity.api.OrderOperator;
import io.github.nextentity.api.Path;
import io.github.nextentity.api.Path.NumberPath;
import io.github.nextentity.api.Path.StringPath;
import io.github.nextentity.api.SubQueryBuilder;
import io.github.nextentity.api.TypedExpression;
import io.github.nextentity.api.TypedExpression.OperatableExpression;
import io.github.nextentity.api.RowsSelectWhereStep;
import io.github.nextentity.api.model.EntityRoot;
import io.github.nextentity.api.model.LockModeType;
import io.github.nextentity.api.model.Order;
import io.github.nextentity.core.expression.Operation;
import io.github.nextentity.core.expression.Operator;
import io.github.nextentity.core.expression.QueryStructure;
import io.github.nextentity.core.expression.QueryStructure.Selected;
import io.github.nextentity.core.expression.QueryStructure.Selected.SelectArray;
import io.github.nextentity.core.expression.QueryStructure.Selected.SelectPrimitive;
import io.github.nextentity.core.expression.impl.ExpressionBuilders;
import io.github.nextentity.core.expression.impl.ExpressionImpls;
import io.github.nextentity.core.expression.Expressions;
import io.github.nextentity.core.util.ImmutableList;
import io.github.nextentity.core.util.Paths;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.List;
import java.util.function.Function;

@SuppressWarnings("PatternVariableCanBeUsed")
public class WhereImpl<T, U> implements RowsSelectWhereStep<T, U>, SelectHavingStep<T, U>, AbstractCollector<U> {

    static final Selected SELECT_ANY = new SelectPrimitive().expression(ExpressionImpls.TRUE).type(Boolean.class);
    static final Selected COUNT_ANY = new SelectPrimitive()
            .expression(ExpressionImpls.operate(ExpressionImpls.TRUE, Operator.COUNT)).type(Long.class);

    protected QueryConfig config;
    protected QueryStructure queryStructure;

    public WhereImpl() {
    }

    public WhereImpl(QueryConfig config, Class<T> type) {
        this(config, ExpressionImpls.queryStructure(type));
    }

    public WhereImpl(QueryConfig config, QueryStructure queryStructure) {
        this.config = config;
        this.queryStructure = queryStructure;
    }

    public void init(QueryConfig config, Class<T> type) {
        this.config = config;
        this.queryStructure = ExpressionImpls.queryStructure(type);
    }

    <X, Y> WhereImpl<X, Y> update(QueryStructure queryStructure) {
        return new WhereImpl<>(config, queryStructure);
    }

    @Override
    public RowsSelectWhereStep<T, U> where(TypedExpression<T, Boolean> predicate) {
        if (ExpressionImpls.isNullOrTrue(predicate)) {
            return this;
        }
        QueryStructure structure = whereAnd(queryStructure, predicate);
        return update(structure);
    }

    static QueryStructure whereAnd(QueryStructure structure, Expression expression) {
        Expression where;
        if (ExpressionImpls.isNullOrTrue(structure.where())) {
            where = expression;
        } else {
            where = ExpressionImpls.operate(structure.where(), Operator.AND, expression);
        }
        return ExpressionImpls.where(structure, where);
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
        List<? extends Order<?>> orderBy = queryStructure.orderBy();
        orderBy = orderBy == null ? orders : ImmutableList.concat(orderBy, orders);
        QueryStructure structure = ExpressionImpls.queryStructure(
                queryStructure.select(),
                queryStructure.from(),
                queryStructure.where(),
                queryStructure.groupBy(),
                orderBy,
                queryStructure.having(),
                queryStructure.offset(),
                queryStructure.limit(),
                queryStructure.lockType()
        );
        return update(structure);
    }

    @Override
    public long count() {
        QueryStructure structure = buildCountData();
        QueryPostProcessor processor = config.queryPostProcessor();
        if (processor != null) {
            structure = processor.preCountQuery(structure);
        }
        return config.queryExecutor().<Number>getList(structure).get(0).longValue();
    }

    @NotNull
    QueryStructure buildCountData() {
        if (queryStructure.select().distinct()) {
            return countFrom(queryStructure.select());
        } else if (requiredCountSubQuery(queryStructure.select())) {
            return countFrom(COUNT_ANY);
        } else if (queryStructure.groupBy() != null && !queryStructure.groupBy().isEmpty()) {
            return countFrom(SELECT_ANY);
        } else {
            return ExpressionImpls.queryStructure(
                    COUNT_ANY,
                    queryStructure.from(),
                    queryStructure.where(),
                    queryStructure.groupBy(),
                    ImmutableList.of(),
                    queryStructure.having(),
                    null,
                    null,
                    LockModeType.NONE);
        }

    }


    public QueryStructure countFrom(Selected selected) {
        QueryStructure.From from = ExpressionImpls.from(ExpressionImpls.queryStructure(
                selected,
                queryStructure.from(),
                queryStructure.where(),
                queryStructure.groupBy(),
                ImmutableList.of(),
                queryStructure.having(),
                null,
                null,
                LockModeType.NONE));
        return ExpressionImpls.queryStructure(COUNT_ANY, from);
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

    protected boolean requiredCountSubQuery(Expression expression) {
        if (expression instanceof Operation) {
            Operation operation = (Operation) expression;
            if (operation.operator().isAgg()) {
                return true;
            }
            List<? extends Expression> args = operation.operands();
            if (args != null) {
                for (Expression arg : args) {
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
        QueryPostProcessor processor = config.queryPostProcessor();
        if (processor != null) {
            structure = processor.preListQuery(structure);
        }
        return queryList(structure);
    }

    public <X> List<X> queryList(QueryStructure structure) {
        return config.queryExecutor().getList(structure);
    }

    @NotNull
    QueryStructure buildListData(int offset, int maxResult, LockModeType lockModeType) {
        return ExpressionImpls.queryStructure(
                queryStructure.select(),
                queryStructure.from(),
                queryStructure.where(),
                queryStructure.groupBy(),
                queryStructure.orderBy(),
                queryStructure.having(),
                offset,
                maxResult,
                lockModeType
        );
    }

    @Override
    public boolean exist(int offset) {
        QueryStructure structure = buildExistData(offset);
        QueryPostProcessor processor = config.queryPostProcessor();
        if (processor != null) {
            structure = processor.preExistQuery(structure);
        }
        return !queryList(structure).isEmpty();
    }

    @NotNull
    QueryStructure buildExistData(int offset) {
        return ExpressionImpls.queryStructure(
                SELECT_ANY,
                queryStructure.from(),
                queryStructure.where(),
                queryStructure.groupBy(),
                ImmutableList.of(),
                queryStructure.having(),
                offset,
                1,
                queryStructure.lockType()
        );
    }

    @Override
    public <X> SubQueryBuilder<X, U> asSubQuery() {
        return new SubQuery<>();
    }

    @Override
    public SelectHavingStep<T, U> groupBy(List<? extends TypedExpression<T, ?>> expressions) {
        return setGroupBy(expressions);
    }

    private WhereImpl<T, U> setGroupBy(List<? extends Expression> group) {
        QueryStructure structure = ExpressionImpls.queryStructure(
                queryStructure.select(),
                queryStructure.from(),
                queryStructure.where(),
                group,
                queryStructure.orderBy(),
                queryStructure.having(),
                queryStructure.offset(),
                queryStructure.limit(),
                queryStructure.lockType()
        );
        return update(structure);
    }

    @Override
    public SelectHavingStep<T, U> groupBy(Path<T, ?> path) {
        return setGroupBy(ImmutableList.of(ExpressionImpls.of(path)));
    }

    @Override
    public SelectHavingStep<T, U> groupBy(Collection<Path<T, ?>> paths) {
        return groupBy(ExpressionImpls.toExpressionList(paths));
    }

    @Override
    public SelectOrderByStep<T, U> having(TypedExpression<T, Boolean> predicate) {
        QueryStructure structure = ExpressionImpls.queryStructure(
                queryStructure.select(),
                queryStructure.from(),
                queryStructure.where(),
                queryStructure.groupBy(),
                queryStructure.orderBy(),
                predicate,
                queryStructure.offset(),
                queryStructure.limit(),
                queryStructure.lockType()
        );
        return update(structure);
    }

    @Override
    public <N extends Number> NumberOperator<T, N, RowsSelectWhereStep<T, U>> where(NumberPath<T, N> path) {
        return ExpressionBuilders.ofNumber(root().get(path), this::whereAnd);
    }

    @NotNull
    private RowsSelectWhereStep<T, U> whereAnd(OperatableExpression<?, ?> expression) {
        if (expression == null) {
            return this;
        }
        QueryStructure structure = whereAnd(queryStructure, expression);
        return update(structure);
    }

    @Override
    public StringOperator<T, RowsSelectWhereStep<T, U>> where(StringPath<T> path) {
        return ExpressionBuilders.ofString(root().get(path), this::whereAnd);
    }

    public EntityRoot<T> root() {
        return Paths.root();
    }

    @Override
    public <N> PathOperator<T, N, RowsSelectWhereStep<T, U>> where(Path<T, N> path) {
        return ExpressionBuilders.ofPath(root().get(path), this::whereAnd);
    }


    class SubQuery<X> implements SubQueryBuilder<X, U>, QueryStructure {

        @Override
        public TypedExpression<X, Long> count() {
            QueryStructure structure = buildCountData();
            return Expressions.of(structure);
        }

        @Override
        public TypedExpression<X, List<U>> slice(int offset, int maxResult) {
            QueryStructure structure = buildListData(offset, maxResult, null);
            return Expressions.of(structure);
        }

        @Override
        public TypedExpression<X, U> getSingle(int offset) {
            QueryStructure structure = buildListData(offset, 2, null);
            return Expressions.of(structure);
        }

        @Override
        public TypedExpression<X, U> getFirst(int offset) {
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

        public Expression having() {
            return queryStructure.having();
        }

        public List<? extends Order<?>> orderBy() {
            return queryStructure.orderBy();
        }

        public List<? extends Expression> groupBy() {
            return queryStructure.groupBy();
        }

        public Expression where() {
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
