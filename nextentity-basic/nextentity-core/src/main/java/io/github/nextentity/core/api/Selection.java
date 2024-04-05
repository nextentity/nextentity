package io.github.nextentity.core.api;

import io.github.nextentity.core.api.Expression.ExpressionTree;
import io.github.nextentity.core.util.tuple.Tuple;

import java.io.Serializable;
import java.util.List;

public interface Selection extends Serializable {

    Class<?> resultType();

    boolean distinct();

    interface MultiSelected extends Selection {
        List<? extends ExpressionTree> expressions();

        @Override
        default Class<?> resultType() {
            return Tuple.class;
        }

    }

    interface SingleSelected extends Selection {
        ExpressionTree expression();

    }

    interface ProjectionSelected extends Selection {
    }

    interface EntitySelected extends Selection {
    }

}
