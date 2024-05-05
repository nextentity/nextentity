package io.github.nextentity.api.model;

import io.github.nextentity.api.Expression;
import io.github.nextentity.api.SortOrder;

import java.io.Serializable;

/**
 * @author HuangChengwei
 * @since 2024/4/17 下午1:27
 */
@SuppressWarnings("unused")
public interface Order<T> extends Serializable {

    Expression expression();

    SortOrder order();

}
