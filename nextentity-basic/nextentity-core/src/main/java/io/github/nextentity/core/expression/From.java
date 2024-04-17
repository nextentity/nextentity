package io.github.nextentity.core.expression;

import java.io.Serializable;

/**
 * @author HuangChengwei
 * @since 2024/4/17 下午1:35
 */
public
interface From extends Serializable {

    Class<?> type();

    interface Entity extends From {

    }

    interface FromSubQuery extends From, QueryStructure {
        @Override
        default Class<?> type() {
            return select().resultType();
        }
    }

}
