package io.github.nextentity.api.model;

import io.github.nextentity.api.Path;
import io.github.nextentity.api.TypedExpression;
import io.github.nextentity.api.TypedExpression.BooleanPathExpression;
import io.github.nextentity.api.TypedExpression.EntityPathExpression;
import io.github.nextentity.api.TypedExpression.NumberPathExpression;
import io.github.nextentity.api.TypedExpression.PathExpression;
import io.github.nextentity.api.TypedExpression.StringPathExpression;

public interface EntityRoot<T> {

    <U> TypedExpression<T, U> literal(U value);

    <U> EntityPathExpression<T, U> get(Path<T, U> path);

    BooleanPathExpression<T> get(Path.BooleanPath<T> path);

    StringPathExpression<T> get(Path.StringPath<T> path);

    <U extends Number> NumberPathExpression<T, U> get(Path.NumberPath<T, U> path);

    <U> PathExpression<T, U> path(Path<T, U> path);

    <U> EntityPathExpression<T, U> entity(Path<T, U> path);

    StringPathExpression<T> string(Path<T, String> path);

    <U extends Number> NumberPathExpression<T, U> number(Path<T, U> path);

    BooleanPathExpression<T> bool(Path<T, Boolean> path);


    // type-unsafe

    <U> PathExpression<T, U> path(String fieldName);

    <U> EntityPathExpression<T, U> entityPath(String fieldName);

    StringPathExpression<T> stringPath(String fieldName);

    <U extends Number> NumberPathExpression<T, U> numberPath(String fieldName);

    BooleanPathExpression<T> booleanPath(String fieldName);

}
