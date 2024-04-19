package io.github.nextentity.core.expression;

import io.github.nextentity.core.api.ExpressionTree.ExpressionNode;

import java.util.List;

/**
 * @author HuangChengwei
 * @since 2024/4/19 下午5:51
 */
public interface Selection {

    boolean distinct();

    Class<?> resultType();

    interface Entity {
        List<Attribute> fetch();
    }

    interface Projection {
    }

    interface Expressions {
        List<ExpressionNode> expressions();

        boolean single();
    }

}
