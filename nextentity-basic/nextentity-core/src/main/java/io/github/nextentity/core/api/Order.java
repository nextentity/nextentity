package io.github.nextentity.core.api;

import java.io.Serializable;

@SuppressWarnings("unused")
public interface Order<T> extends Serializable {

    Expression expression();

    SortOrder order();

    enum SortOrder {
        ASC, DESC
    }
}
