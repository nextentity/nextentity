package io.github.nextentity.core.util;

import io.github.nextentity.api.Path;
import io.github.nextentity.api.Path.BooleanPath;
import io.github.nextentity.api.Path.NumberPath;
import io.github.nextentity.api.Path.StringPath;
import io.github.nextentity.api.TypedExpression;
import io.github.nextentity.api.TypedExpression.BooleanPathExpression;
import io.github.nextentity.api.TypedExpression.EntityPathExpression;
import io.github.nextentity.api.TypedExpression.NumberPathExpression;
import io.github.nextentity.api.TypedExpression.PathExpression;
import io.github.nextentity.api.TypedExpression.StringPathExpression;
import io.github.nextentity.api.model.EntityRoot;
import io.github.nextentity.core.TypeCastUtil;
import io.github.nextentity.core.expression.impl.ExpressionImpls;
import io.github.nextentity.core.expression.Expressions;

public interface Paths {

    static <T> EntityRoot<T> root() {
        return RootImpl.of();
    }

    static <T, U> EntityPathExpression<T, U> get(Path<T, U> path) {
        return Paths.<T>root().entity(path);
    }

    static <T> BooleanPathExpression<T> get(BooleanPath<T> path) {
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

    static <T> BooleanPathExpression<T> bool(Path<T, Boolean> path) {
        return Paths.<T>root().bool(path);
    }

    // type-unsafe

    static <T, U> PathExpression<T, U> path(String fieldName) {
        return Paths.<T>root().path(fieldName);
    }

    static <T, U> EntityPathExpression<T, U> entityPath(String fieldName) {
        return Paths.<T>root().entityPath(fieldName);
    }

    static <T> StringPathExpression<T> stringPath(String fieldName) {
        return Paths.<T>root().stringPath(fieldName);
    }

    static <T, U extends Number> NumberPathExpression<T, U> numberPath(String fieldName) {
        return Paths.<T>root().numberPath(fieldName);
    }

    static <T> BooleanPathExpression<T> booleanPath(String fieldName) {
        return Paths.<T>root().booleanPath(fieldName);
    }

    class RootImpl<T> implements EntityRoot<T> {

        private static final RootImpl<?> INSTANCE = new RootImpl<>();

        public static <T> EntityRoot<T> of() {
            return TypeCastUtil.cast(INSTANCE);
        }

        protected RootImpl() {
        }

        @Override
        public <U> TypedExpression<T, U> literal(U value) {
            return Expressions.of(value);
        }

        @Override
        public <U> PathExpression<T, U> path(Path<T, U> path) {
            return Expressions.ofPath(ExpressionImpls.of(path));
        }

        @Override
        public <U> EntityPathExpression<T, U> entity(Path<T, U> path) {
            return Expressions.ofEntity(ExpressionImpls.of(path));
        }

        @Override
        public <U> EntityPathExpression<T, U> get(Path<T, U> path) {
            return Expressions.ofEntity(ExpressionImpls.of(path));
        }

        @Override
        public BooleanPathExpression<T> get(BooleanPath<T> path) {
            return Expressions.ofBoolean(ExpressionImpls.of(path));
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
            return Expressions.ofString(ExpressionImpls.of(path));
        }

        @Override
        public <U extends Number> NumberPathExpression<T, U> number(Path<T, U> path) {
            return Expressions.ofNumber(ExpressionImpls.of(path));
        }

        @Override
        public BooleanPathExpression<T> bool(Path<T, Boolean> path) {
            return Expressions.ofBoolean(ExpressionImpls.of(path));
        }

        @Override
        public <U> PathExpression<T, U> path(String fieldName) {
            return Expressions.ofPath(ExpressionImpls.column(fieldName));
        }

        @Override
        public <U> EntityPathExpression<T, U> entityPath(String fieldName) {
            return Expressions.ofEntity(ExpressionImpls.column(fieldName));
        }

        @Override
        public StringPathExpression<T> stringPath(String fieldName) {
            return Expressions.ofString(ExpressionImpls.column(fieldName));
        }

        @Override
        public <U extends Number> NumberPathExpression<T, U> numberPath(String fieldName) {
            return Expressions.ofNumber(ExpressionImpls.column(fieldName));
        }

        @Override
        public BooleanPathExpression<T> booleanPath(String fieldName) {
            return Expressions.ofBoolean(ExpressionImpls.column(fieldName));
        }

    }
}
