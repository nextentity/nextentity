package io.github.nextentity.core.expression;

import io.github.nextentity.core.util.Lists;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * @author HuangChengwei
 * @since 2024/4/17 下午4:05
 */
@Data
@Accessors(fluent = true)
public
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
