package io.github.nextentity.core.api;

import io.github.nextentity.core.api.ExpressionTree.ExpressionNode;

import java.io.Serializable;

/**
 * @author HuangChengwei
 * @since 2024/4/17 下午1:27
 */
public
interface Order<T> extends Serializable {

    ExpressionNode expression();

    SortOrder order();

}
