package io.github.nextentity.core.meta;

import io.github.nextentity.core.reflect.Arguments;
import io.github.nextentity.core.reflect.ObjectFactory;
import io.github.nextentity.core.reflect.SelectedConstruct;

/**
 * @author HuangChengwei
 * @since 2024/4/20 下午5:34
 */
public interface EntityType extends EntitySchema, ObjectFactory {

    ProjectionType getProjection(Class<?> type);

    SelectedConstruct constructor();

    @Override
    default Object get(Arguments arguments) {
        return constructor().get(arguments);
    }
}
