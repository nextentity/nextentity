package io.github.nextentity.core.meta;

/**
 * @author HuangChengwei
 * @since 2024/4/20 下午5:34
 */
public interface EntityType extends EntitySchema {

    ProjectionType getProjection(Class<?> type);

}
