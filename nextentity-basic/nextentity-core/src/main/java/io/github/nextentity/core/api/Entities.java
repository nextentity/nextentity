package io.github.nextentity.core.api;

import io.github.nextentity.core.api.Query.Select;

/**
 * @author HuangChengwei
 * @since 2024-04-08 11:47
 */
public interface Entities<T> extends Select<T>, Updater<T>, EntityRoot<T> {

}
