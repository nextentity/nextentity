package io.github.nextentity.core;

import io.github.nextentity.core.api.ExpressionTree.QueryStructure.Order;
import io.github.nextentity.core.api.LockModeType;
import io.github.nextentity.core.api.Page;
import io.github.nextentity.core.api.Pageable;
import io.github.nextentity.core.api.Path;
import io.github.nextentity.core.api.Query.Collector;
import io.github.nextentity.core.api.Query.OrderBy;
import io.github.nextentity.core.api.Query.OrderOperator;
import io.github.nextentity.core.api.Query.QueryStructureBuilder;
import io.github.nextentity.core.api.Query.SubQueryBuilder;
import io.github.nextentity.core.api.EntityRoot;
import io.github.nextentity.core.api.Slice;
import io.github.nextentity.core.api.Sliceable;
import io.github.nextentity.core.api.SortOrder;
import io.github.nextentity.core.util.Paths;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

public class OrderOperatorImpl<T, U> implements OrderOperator<T, U> {
    private final QueryConditionBuilder<T, U> builder;
    private final Collection<Path<T, Comparable<?>>> orderByPaths;

    public OrderOperatorImpl(QueryConditionBuilder<T, U> builder, Collection<Path<T, Comparable<?>>> orderByPaths) {
        this.builder = builder;
        this.orderByPaths = orderByPaths;
    }

    @NotNull
    private List<Order<T>> asOrderList(SortOrder sort) {
        return orderByPaths
                .stream()
                .map(path -> Paths.get(path).sort(sort))
                .collect(Collectors.toList());
    }

    @Override
    public OrderBy<T, U> sort(SortOrder order) {
        return builder.addOrderBy(asOrderList(order));
    }

    @Override
    public Collector<U> orderBy(List<? extends Order<T>> orders) {
        return asc().orderBy(orders);
    }

    @Override
    public Collector<U> orderBy(Function<EntityRoot<T>, List<? extends Order<T>>> ordersBuilder) {
        return orderBy(ordersBuilder.apply(Paths.root()));
    }

    @Override
    public OrderOperator<T, U> orderBy(Collection<Path<T, Comparable<?>>> paths) {
        return asc().orderBy(paths);
    }

    @Override
    public long count() {
        return asc().count();
    }

    @Override
    public List<U> getList(int offset, int maxResult, LockModeType lockModeType) {
        return asc().getList(offset, maxResult, lockModeType);
    }

    @Override
    public boolean exist(int offset) {
        return asc().exist(offset);
    }

    @Override
    public <R> R slice(Sliceable<U, R> sliceable) {
        return asc().slice(sliceable);
    }

    @Override
    public Slice<U> slice(int offset, int limit) {
        return asc().slice(offset, limit);
    }

    @Override
    public QueryStructureBuilder buildMetadata() {
        return asc().buildMetadata();
    }

    @Override
    public <X> SubQueryBuilder<X, U> asSubQuery() {
        return asc().asSubQuery();
    }

    @Override
    public Page<U> getPage(Pageable pageable) {
        return asc().getPage(pageable);
    }

    @Override
    public EntityRoot<T> root() {
        return Paths.root();
    }
}
