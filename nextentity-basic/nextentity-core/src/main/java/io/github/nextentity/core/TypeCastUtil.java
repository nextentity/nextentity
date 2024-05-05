package io.github.nextentity.core;

import io.github.nextentity.api.Expression;
import io.github.nextentity.api.TypedExpression;
import io.github.nextentity.api.model.EntityRoot;

import java.util.List;

public class TypeCastUtil {

    public static <T> List<T> cast(List<?> expression) {
        return unsafeCast(expression);
    }

    public static <T> Class<T> cast(Class<?> resolve) {
        return unsafeCast(resolve);
    }

    public static <T, U> TypedExpression<T, U> cast(Expression expression) {
        return unsafeCast(expression);
    }

    public static <T> EntityRoot<T> cast(EntityRoot<?> builder) {
        return unsafeCast(builder);
    }

    public static <T> T unsafeCast(Object object) {
        // noinspection unchecked
        return (T) object;
    }

}
