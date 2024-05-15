package io.github.nextentity.api;

import io.github.nextentity.api.model.EntityRoot;
import io.github.nextentity.api.model.Order;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.function.Function;

/**
 * @author HuangChengwei
 * @since 2024-05-06 8:38
 */
public
interface SelectOrderByStep<T, U> extends Collector<U>, EntityRootProvider<T> {

    Collector<U> orderBy(List<? extends Order<T>> orders);

    Collector<U> orderBy(Function<EntityRoot<T>, List<? extends Order<T>>> ordersBuilder);

    default Collector<U> orderBy(Order<T> order) {
        return orderBy(Collections.singletonList(order));
    }

    default Collector<U> orderBy(Order<T> p0, Order<T> p1) {
        return orderBy(Arrays.asList(p0, p1));
    }

    default Collector<U> orderBy(Order<T> order1, Order<T> order2, Order<T> order3) {
        return orderBy(Arrays.asList(order1, order2, order3));
    }

    OrderOperator<T, U> orderBy(Collection<Path<T, Comparable<?>>> paths);

    default OrderOperator<T, U> orderBy(Path<T, Comparable<?>> path) {
        return orderBy(Collections.singletonList(path));
    }

    default OrderOperator<T, U> orderBy(Path<T, Comparable<?>> p1, Path<T, Comparable<?>> p2) {
        return orderBy(Arrays.asList(p1, p2));
    }

    default OrderOperator<T, U> orderBy(Path<T, Comparable<?>> p1, Path<T, Comparable<?>> p2, Path<T, Comparable<?>> p3) {
        return orderBy(Arrays.asList(p1, p2, p3));
    }

}
