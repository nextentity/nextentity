package io.github.nextentity.core.expression;

import io.github.nextentity.core.meta.Attribute;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * @author HuangChengwei
 * @since 2024/4/17 下午3:21
 */
@Data
@Accessors(fluent = true)
public class SelectEntity implements SelectElement {
    private Class<?> javaType;
    private List<Attribute> attributes;
}
