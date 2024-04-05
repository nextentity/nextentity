package io.github.nextentity.core.api;

import io.github.nextentity.core.api.Expression.ExpressionTree;

import java.io.Serializable;

@SuppressWarnings("unused")
public interface Order<T> extends Serializable {

    ExpressionTree expression();

    SortOrder order();

    enum SortOrder {
        ASC, DESC
    }
}
