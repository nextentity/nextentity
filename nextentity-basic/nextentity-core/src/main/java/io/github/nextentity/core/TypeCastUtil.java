package io.github.nextentity.core;

import io.github.nextentity.core.api.EntityRoot;
import io.github.nextentity.core.api.Expression;
import io.github.nextentity.core.api.expression.BaseExpression;

import java.util.List;

public class TypeCastUtil {

    public static <T> List<T> cast(List<?> expression) {
        return unsafeCast(expression);
    }

    public static <T> Class<T> cast(Class<?> resolve) {
        return unsafeCast(resolve);
    }

    public static <T, U> Expression<T, U> cast(BaseExpression expression) {
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
