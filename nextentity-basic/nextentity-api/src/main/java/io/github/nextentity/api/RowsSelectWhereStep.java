package io.github.nextentity.api;

import io.github.nextentity.api.ExpressionBuilder.NumberOperator;
import io.github.nextentity.api.ExpressionBuilder.PathOperator;
import io.github.nextentity.api.ExpressionBuilder.StringOperator;
import io.github.nextentity.api.Path.NumberPath;
import io.github.nextentity.api.Path.StringPath;

/**
 * @author HuangChengwei
 * @since 2024-05-06 8:39
 */
public
interface RowsSelectWhereStep<T, U> extends SelectGroupByStep<T, U>, SelectWhereStep<T, U> {

    RowsSelectWhereStep<T, U> where(TypedExpression<T, Boolean> predicate);

    <N> PathOperator<T, N, RowsSelectWhereStep<T, U>> where(Path<T, N> path);

    <N extends Number> NumberOperator<T, N, RowsSelectWhereStep<T, U>> where(NumberPath<T, N> path);

    StringOperator<T, RowsSelectWhereStep<T, U>> where(StringPath<T> path);

}
