package io.github.nextentity.core.expression;

import io.github.nextentity.core.api.LockModeType;
import io.github.nextentity.core.api.Order;
import io.github.nextentity.core.api.ExpressionTree.ExpressionNode;

import java.util.List;

/**
 * @author HuangChengwei
 * @since 2024/4/17 下午1:28
 */
public interface QueryStructure extends ExpressionNode {

    Selection select();

    From from();

    ExpressionNode where();

    List<? extends ExpressionNode> groupBy();

    List<? extends Order<?>> orderBy();

    ExpressionNode having();

    Integer offset();

    Integer limit();

    LockModeType lockType();

    List<? extends PathChain> fetch();

}
