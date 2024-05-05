package io.github.nextentity.api;

/**
 * @author HuangChengwei
 * @since 2024-05-06 8:38
 */
public
interface OrderOperator<T, U> extends SelectOrderByStep<T, U> {
    default SelectOrderByStep<T, U> asc() {
        return sort(SortOrder.ASC);
    }

    default SelectOrderByStep<T, U> desc() {
        return sort(SortOrder.DESC);
    }

    SelectOrderByStep<T, U> sort(SortOrder order);
}
