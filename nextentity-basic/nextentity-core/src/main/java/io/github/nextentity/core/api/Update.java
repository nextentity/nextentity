package io.github.nextentity.core.api;

import io.github.nextentity.core.Updaters;
import org.jetbrains.annotations.NotNull;

import java.util.List;

public interface Update {

    default <T> T insert(@NotNull T entity, @NotNull Class<T> entityType) {
        return insert(Lists.of(entity), entityType).get(0);
    }

    <T> List<T> insert(@NotNull Iterable<T> entities, @NotNull Class<T> entityType);

    <T> List<T> update(@NotNull Iterable<T> entities, @NotNull Class<T> entityType);

    default <T> T update(@NotNull T entity, Class<T> entityType) {
        return update(Lists.of(entity), entityType).get(0);
    }

    <T> void delete(@NotNull Iterable<T> entities, @NotNull Class<T> entityType);

    default <T> void delete(@NotNull T entity, @NotNull Class<T> entityType) {
        delete(Lists.of(entity), entityType);
    }

    <T> T updateNonNullColumn(@NotNull T entity, @NotNull Class<T> entityType);

    /**
     * @deprecated use {@link Updaters#create}
     */
    @Deprecated
    <T> Updater<T> getUpdater(@NotNull Class<T> type);

}
