package io.github.nextentity.core.api;

import java.io.Serializable;

public interface ExpressionTree extends Serializable {
    ExpressionNode rootNode();

    interface ExpressionNode extends Serializable, ExpressionTree {
        @Override
        default ExpressionNode rootNode() {
            return this;
        }

    }
}
