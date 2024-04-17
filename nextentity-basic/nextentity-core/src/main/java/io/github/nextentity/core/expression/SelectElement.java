package io.github.nextentity.core.expression;

/**
 * @author HuangChengwei
 * @since 2024/4/17 下午3:21
 */
public interface SelectElement {
    default Class<?> javaType() {
        return Object.class;
    }
}
