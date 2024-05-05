package io.github.nextentity.api;

import io.github.nextentity.api.model.EntityRoot;

/**
 * @author HuangChengwei
 * @since 2024-05-06 8:37
 */
public interface EntityRootProvider<T> {
    EntityRoot<T> root();
}
