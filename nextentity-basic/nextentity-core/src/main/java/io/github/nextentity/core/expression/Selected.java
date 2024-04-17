package io.github.nextentity.core.expression;

import io.github.nextentity.core.api.ExpressionTree;
import io.github.nextentity.core.api.ExpressionTree.ExpressionNode;

import java.io.Serializable;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author HuangChengwei
 * @since 2024/4/17 下午1:35
 */
public
interface Selected extends Serializable {

    Class<?> resultType();

    boolean distinct();

    List<? extends SelectElement> elements();

    default List<? extends ExpressionNode> expressions() {
        return elements().stream()
                .flatMap(selectElement -> {
                    if (selectElement instanceof ExpressionTree) {
                        return Stream.of(((ExpressionTree) selectElement).rootNode());
                    } else if (selectElement instanceof SelectEntity) {
                        return ((SelectEntity) selectElement).columns().stream();
                    } else {
                        throw new UnsupportedOperationException(selectElement.getClass().getName());
                    }
                })
                .collect(Collectors.toList());
    }

}
