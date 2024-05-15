package io.github.nextentity.core;

import io.github.nextentity.core.util.ImmutableList;
import org.jetbrains.annotations.NotNull;

import java.util.List;

/**
 * @author HuangChengwei
 * @since 2024-05-06 13:55
 */
public interface UpdateExecutor {

    default <T> T insert(@NotNull T entity, @NotNull Class<T> entityType) {
        return insert(ImmutableList.of(entity), entityType).get(0);
    }

    <T> List<T> insert(@NotNull Iterable<T> entities, @NotNull Class<T> entityType);

    <T> List<T> update(@NotNull Iterable<T> entities, @NotNull Class<T> entityType);

    default <T> T update(@NotNull T entity, Class<T> entityType) {
        return update(ImmutableList.of(entity), entityType).get(0);
    }

    <T> void delete(@NotNull Iterable<T> entities, @NotNull Class<T> entityType);

    default <T> void delete(@NotNull T entity, @NotNull Class<T> entityType) {
        delete(ImmutableList.of(entity), entityType);
    }

    <T> T updateExcludeNullColumn(@NotNull T entity, @NotNull Class<T> entityType);

}
