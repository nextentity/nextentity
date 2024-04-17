package io.github.nextentity.core.expression;

import io.github.nextentity.core.api.ExpressionTree.ExpressionNode;
import io.github.nextentity.core.util.tuple.Tuple;

import java.io.Serializable;
import java.util.List;

/**
 * @author HuangChengwei
 * @since 2024/4/17 下午1:35
 */
public
interface Selection extends Serializable {

    Class<?> resultType();

    boolean distinct();

    interface MultiSelected extends Selection {
        List<? extends ExpressionNode> expressions();

        @Override
        default Class<?> resultType() {
            return Tuple.class;
        }

    }

    interface SingleSelected extends Selection {
        ExpressionNode expression();

    }

    interface ProjectionSelected extends Selection {
    }

    interface EntitySelected extends Selection {
    }


    //    interface MultiSelected extends Selection {
    //         List<? extends SelectElement> elements();
    //
    //         @Override
    //         default Class<?> resultType() {
    //             return Tuple.class;
    //         }
    //     }
    //
    //     interface SingleSelected extends Selection {
    //         SelectElement element();
    //
    //         @Override
    //         default Class<?> resultType() {
    //             return element().javaType();
    //         }
    //     }
}
