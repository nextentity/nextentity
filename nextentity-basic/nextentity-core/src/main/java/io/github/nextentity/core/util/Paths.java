package io.github.nextentity.core.util;

import io.github.nextentity.core.RootImpl;
import io.github.nextentity.core.api.Path;
import io.github.nextentity.core.api.Path.BooleanPath;
import io.github.nextentity.core.api.Path.ComparablePath;
import io.github.nextentity.core.api.Path.NumberPath;
import io.github.nextentity.core.api.Path.StringPath;
import io.github.nextentity.core.api.TypedExpression.BooleanPathExpression;
import io.github.nextentity.core.api.TypedExpression.ComparablePathExpression;
import io.github.nextentity.core.api.TypedExpression.EntityPathExpression;
import io.github.nextentity.core.api.TypedExpression.NumberPathExpression;
import io.github.nextentity.core.api.TypedExpression.StringPathExpression;

public interface Paths {

    static <T, U> EntityPathExpression<T, U> get(Path<T, U> path) {
        return RootImpl.<T>of().entity(path);
    }

    static <T> StringPathExpression<T> get(StringPath<T> path) {
        return RootImpl.<T>of().get(path);
    }

    static <T, U extends Number & Comparable<U>> NumberPathExpression<T, U> get(NumberPath<T, U> path) {
        return RootImpl.<T>of().get(path);
    }

    static <T, V extends Comparable<V>> ComparablePathExpression<T, V> get(ComparablePath<T, V> path) {
        return RootImpl.<T>of().get(path);
    }

    static <T> BooleanPathExpression<T> get(BooleanPath<T> path) {
        return RootImpl.<T>of().get(path);
    }

    static <T, U> EntityPathExpression<T, U> entity(Path<T, U> path) {
        return RootImpl.<T>of().entity(path);
    }

    static <T> StringPathExpression<T> string(Path<T, String> path) {
        return RootImpl.<T>of().string(path);
    }

    static <T, U extends Number & Comparable<U>> NumberPathExpression<T, U> number(Path<T, U> path) {
        return RootImpl.<T>of().number(path);
    }

    static <T, U extends Comparable<U>> ComparablePathExpression<T, U> comparable(Path<T, U> path) {
        return RootImpl.<T>of().comparable(path);
    }

    static <T> BooleanPathExpression<T> bool(Path<T, Boolean> path) {
        return RootImpl.<T>of().bool(path);
    }

}
