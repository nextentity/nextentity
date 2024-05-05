package io.github.nextentity.api;

import java.util.List;

/**
 * @author HuangChengwei
 * @since 2024-05-06 8:38
 */
public
interface SubQueryBuilder<T, U> extends TypedExpression<T, List<U>> {
    TypedExpression<T, Long> count();

    TypedExpression<T, List<U>> slice(int offset, int maxResult);

    default TypedExpression<T, U> getSingle() {
        return getSingle(-1);
    }

    TypedExpression<T, U> getSingle(int offset);

    default TypedExpression<T, U> getFirst() {
        return getFirst(-1);
    }

    TypedExpression<T, U> getFirst(int offset);
}
