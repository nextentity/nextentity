package io.github.nextentity.core.util;

import io.github.nextentity.core.Expressions;
import io.github.nextentity.core.TypeCastUtil;
import io.github.nextentity.core.TypedExpressions;
import io.github.nextentity.core.api.Path;
import io.github.nextentity.core.api.Path.BooleanPath;
import io.github.nextentity.core.api.Path.ComparablePath;
import io.github.nextentity.core.api.Path.NumberPath;
import io.github.nextentity.core.api.Path.StringPath;
import io.github.nextentity.core.api.Root;
import io.github.nextentity.core.api.TypedExpression;
import io.github.nextentity.core.api.TypedExpression.BooleanPathExpression;
import io.github.nextentity.core.api.TypedExpression.ComparablePathExpression;
import io.github.nextentity.core.api.TypedExpression.EntityPathExpression;
import io.github.nextentity.core.api.TypedExpression.NumberPathExpression;
import io.github.nextentity.core.api.TypedExpression.StringPathExpression;

public interface Paths {

    static <T, U> EntityPathExpression<T, U> get(Path<T, U> path) {
        return Paths.<T>root().entity(path);
    }

    static <T> Root<T> root() {
        return RootImpl.of();
    }

    static <T> StringPathExpression<T> get(StringPath<T> path) {
        return Paths.<T>root().get(path);
    }

    static <T, U extends Number & Comparable<U>> NumberPathExpression<T, U> get(NumberPath<T, U> path) {
        return Paths.<T>root().get(path);
    }

    static <T, V extends Comparable<V>> ComparablePathExpression<T, V> get(ComparablePath<T, V> path) {
        return Paths.<T>root().get(path);
    }

    static <T> BooleanPathExpression<T> get(BooleanPath<T> path) {
        return Paths.<T>root().get(path);
    }

    static <T, U> EntityPathExpression<T, U> entity(Path<T, U> path) {
        return Paths.<T>root().entity(path);
    }

    static <T> StringPathExpression<T> string(Path<T, String> path) {
        return Paths.<T>root().string(path);
    }

    static <T, U extends Number & Comparable<U>> NumberPathExpression<T, U> number(Path<T, U> path) {
        return Paths.<T>root().number(path);
    }

    static <T, U extends Comparable<U>> ComparablePathExpression<T, U> comparable(Path<T, U> path) {
        return Paths.<T>root().comparable(path);
    }

    static <T> BooleanPathExpression<T> bool(Path<T, Boolean> path) {
        return Paths.<T>root().bool(path);
    }

    class RootImpl<T> implements Root<T> {

        private static final RootImpl<?> INSTANCE = new RootImpl<>();

        public static <T> Root<T> of() {
            return TypeCastUtil.cast(INSTANCE);
        }

        protected RootImpl() {
        }

        @Override
        public <U> TypedExpression<T, U> of(U value) {
            return TypedExpressions.of(value);
        }

        @Override
        public <U> EntityPathExpression<T, U> entity(Path<T, U> path) {
            return TypedExpressions.ofEntity(Expressions.of(path));
        }

        @Override
        public <U> EntityPathExpression<T, U> get(Path<T, U> path) {
            return TypedExpressions.ofEntity(Expressions.of(path));
        }

        @Override
        public StringPathExpression<T> get(StringPath<T> path) {
            return string(path);
        }

        @Override
        public <U extends Number & Comparable<U>> NumberPathExpression<T, U> get(NumberPath<T, U> path) {
            return number(path);
        }

        @Override
        public <U extends Comparable<U>> ComparablePathExpression<T, U> get(ComparablePath<T, U> path) {
            return comparable(path);
        }

        @Override
        public BooleanPathExpression<T> get(BooleanPath<T> path) {
            return bool(path);
        }

        @Override
        public StringPathExpression<T> string(Path<T, String> path) {
            return TypedExpressions.ofString(Expressions.of(path));
        }

        @Override
        public <U extends Number & Comparable<U>> NumberPathExpression<T, U> number(Path<T, U> path) {
            return TypedExpressions.ofNumber(Expressions.of(path));
        }

        @Override
        public <U extends Comparable<U>> ComparablePathExpression<T, U> comparable(Path<T, U> path) {
            return TypedExpressions.ofComparable(Expressions.of(path));
        }

        @Override
        public BooleanPathExpression<T> bool(Path<T, Boolean> path) {
            return TypedExpressions.ofBoolean(Expressions.of(path));
        }

    }
}
