package io.github.nextentity.core.api;

import java.io.Serializable;

public interface From extends Serializable {

    Class<?> type();

    interface Entity extends From {

    }

    interface FromSubQuery extends From, Expression.QueryStructure {
        @Override
        default Class<?> type() {
            return select().resultType();
        }
    }

}
