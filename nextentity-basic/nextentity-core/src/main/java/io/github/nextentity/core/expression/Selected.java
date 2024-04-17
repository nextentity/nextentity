package io.github.nextentity.core.expression;

import io.github.nextentity.core.util.Lists;
import io.github.nextentity.core.util.tuple.Tuple;
import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * @author HuangChengwei
 * @since 2024/4/17 下午1:35
 */
public
interface Selected extends Serializable {

    Class<?> resultType();

    boolean distinct();

    List<? extends SelectElement> elements();


    @Data
    @Accessors(fluent = true)
    class MultiSelected implements Selected {
        private final boolean distinct;
        private final List<? extends SelectElement> elements;

        @Override
        public Class<?> resultType() {
            return Tuple.class;
        }

    }

    @Data
    @Accessors(fluent = true)
    class SingleSelected implements Selected {
        private final SelectElement element;
        private final boolean distinct;

        @Override
        public Class<?> resultType() {
            return element.javaType();
        }

        public List<? extends SelectElement> elements() {
            return Lists.of(element);
        }


    }
}
