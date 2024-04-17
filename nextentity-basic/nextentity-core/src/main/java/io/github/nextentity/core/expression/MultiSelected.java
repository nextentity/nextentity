package io.github.nextentity.core.expression;

import io.github.nextentity.core.util.tuple.Tuple;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * @author HuangChengwei
 * @since 2024/4/17 下午4:04
 */
@Data
@Accessors(fluent = true)
public class MultiSelected implements Selected {
    private final List<? extends SelectElement> elements;
    private final boolean distinct;

    @Override
    public Class<?> resultType() {
        return Tuple.class;
    }

}
