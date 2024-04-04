package io.github.nextentity.core.util;

import io.github.nextentity.core.Expressions;
import io.github.nextentity.core.TypeCastUtil;
import io.github.nextentity.core.TypedExpressions;
import io.github.nextentity.core.api.Path;
import io.github.nextentity.core.api.Path.BooleanPath;
import io.github.nextentity.core.api.Path.NumberPath;
import io.github.nextentity.core.api.Path.StringPath;
import io.github.nextentity.core.api.Root;
import io.github.nextentity.core.api.TypedExpression;
import io.github.nextentity.core.api.TypedExpression.BooleanPathExpression;
import io.github.nextentity.core.api.TypedExpression.EntityPathExpression;
import io.github.nextentity.core.api.TypedExpression.NumberPathExpression;
import io.github.nextentity.core.api.TypedExpression.PathExpression;
import io.github.nextentity.core.api.TypedExpression.Predicate;
import io.github.nextentity.core.api.TypedExpression.StringPathExpression;

public interface Paths {

    static <T> Root<T> root() {
        return RootImpl.of();
    }

    static <T, U> EntityPathExpression<T, U> get(Path<T, U> path) {
        return Paths.<T>root().entity(path);
    }

    static <T, U> BooleanPathExpression<T> get(BooleanPath<T> path) {
        return Paths.<T>root().get(path);
    }

    static <T> StringPathExpression<T> get(StringPath<T> path) {
        return Paths.<T>root().get(path);
    }

    static <T, U extends Number> NumberPathExpression<T, U> get(NumberPath<T, U> path) {
        return Paths.<T>root().get(path);
    }

    static <T, U> PathExpression<T, U> path(Path<T, U> path) {
        return Paths.<T>root().get(path);
    }

    static <T, U> EntityPathExpression<T, U> entity(Path<T, U> path) {
        return Paths.<T>root().entity(path);
    }

    static <T> StringPathExpression<T> string(Path<T, String> path) {
        return Paths.<T>root().string(path);
    }

    static <T, U extends Number> NumberPathExpression<T, U> number(Path<T, U> path) {
        return Paths.<T>root().number(path);
    }

    static <T, U> Predicate<T> predicate(Path<T, Boolean> path) {
        return Paths.<T>root().predicate(path);
    }

    class RootImpl<T> implements Root<T> {

        private static final RootImpl<?> INSTANCE = new RootImpl<>();

        public static <T> Root<T> of() {
            return TypeCastUtil.cast(INSTANCE);
        }

        protected RootImpl() {
        }

        @Override
        public <U> TypedExpression<T, U> literal(U value) {
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
        public BooleanPathExpression<T> get(BooleanPath<T> path) {
            return TypedExpressions.ofBoolean(Expressions.of(path));
        }

        @Override
        public StringPathExpression<T> get(StringPath<T> path) {
            return string(path);
        }

        @Override
        public <U extends Number> NumberPathExpression<T, U> get(NumberPath<T, U> path) {
            return number(path);
        }

        @Override
        public StringPathExpression<T> string(Path<T, String> path) {
            return TypedExpressions.ofString(Expressions.of(path));
        }

        @Override
        public <U extends Number> NumberPathExpression<T, U> number(Path<T, U> path) {
            return TypedExpressions.ofNumber(Expressions.of(path));
        }

        @Override
        public Predicate<T> predicate(Path<T, Boolean> path) {
            return TypedExpressions.ofBoolean(Expressions.of(path));
        }
    }
}
