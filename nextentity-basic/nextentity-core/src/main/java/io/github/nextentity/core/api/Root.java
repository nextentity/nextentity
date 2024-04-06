package io.github.nextentity.core.api;

import io.github.nextentity.core.api.Path.BooleanPath;
import io.github.nextentity.core.api.Path.NumberPath;
import io.github.nextentity.core.api.Path.StringPath;
import io.github.nextentity.core.api.TypedExpression.BooleanPathExpression;
import io.github.nextentity.core.api.TypedExpression.EntityPathExpression;
import io.github.nextentity.core.api.TypedExpression.NumberPathExpression;
import io.github.nextentity.core.api.TypedExpression.StringPathExpression;

public interface Root<T> {

    <U> TypedExpression<T, U> literal(U value);

    <U> EntityPathExpression<T, U> get(Path<T, U> path);

    BooleanPathExpression<T> get(BooleanPath<T> path);

    StringPathExpression<T> get(StringPath<T> path);

    <U extends Number> NumberPathExpression<T, U> get(NumberPath<T, U> path);

    <U> EntityPathExpression<T, U> entity(Path<T, U> path);

    StringPathExpression<T> string(Path<T, String> path);

    <U extends Number> NumberPathExpression<T, U> number(Path<T, U> path);

    BooleanPathExpression<T> bool(Path<T, Boolean> path);
}
