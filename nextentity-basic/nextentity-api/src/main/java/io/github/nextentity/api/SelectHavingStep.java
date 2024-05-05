package io.github.nextentity.api;

/**
 * @author HuangChengwei
 * @since 2024-05-06 8:38
 */
public
interface SelectHavingStep<T, U> extends SelectOrderByStep<T, U> {

    SelectOrderByStep<T, U> having(TypedExpression<T, Boolean> predicate);

}
