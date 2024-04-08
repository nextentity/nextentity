package io.github.nextentity.core.api;

import io.github.nextentity.core.api.Expression.BooleanPathExpression;
import io.github.nextentity.core.api.Expression.EntityPathExpression;
import io.github.nextentity.core.api.Expression.NumberPathExpression;
import io.github.nextentity.core.api.Expression.PathExpression;
import io.github.nextentity.core.api.Expression.StringPathExpression;
import io.github.nextentity.core.api.Path.BooleanPath;
import io.github.nextentity.core.api.Path.NumberPath;
import io.github.nextentity.core.api.Path.StringPath;

public interface EntityRoot<T> {

    <U> Expression<T, U> literal(U value);

    <U> EntityPathExpression<T, U> get(Path<T, U> path);

    BooleanPathExpression<T> get(BooleanPath<T> path);

    StringPathExpression<T> get(StringPath<T> path);

    <U extends Number> NumberPathExpression<T, U> get(NumberPath<T, U> path);

    <U> PathExpression<T, U> path(Path<T, U> path);

    <U> EntityPathExpression<T, U> entity(Path<T, U> path);

    StringPathExpression<T> string(Path<T, String> path);

    <U extends Number> NumberPathExpression<T, U> number(Path<T, U> path);

    BooleanPathExpression<T> bool(Path<T, Boolean> path);
}
