package io.github.nextentity.core.api;

import java.io.Serializable;

sealed public interface Expression extends TypedExpression<Object, Object>, Serializable permits Constant, Column, Operation, SubQuery {
    @Override
    default Expression expression() {
        return this;
    }

}
