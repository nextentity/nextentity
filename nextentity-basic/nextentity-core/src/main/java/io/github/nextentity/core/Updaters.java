package io.github.nextentity.core;

import io.github.nextentity.core.api.Update;
import io.github.nextentity.core.util.ImmutableList;
import org.jetbrains.annotations.NotNull;

import java.util.List;

/**
 * @author HuangChengwei
 * @since 2024-04-02 8:11
 */
public class Updaters {

    public static <T> Update<T> create(UpdateExecutor updateExecutor, Class<T> type) {
        return new UpdateImpl<>(updateExecutor, type);
    }

    public static class UpdateImpl<T> implements Update<T> {
        private final UpdateExecutor update;
        private final Class<T> entityType;

        public UpdateImpl(UpdateExecutor update, Class<T> entityType) {
            this.entityType = entityType;
            this.update = update;
        }

        @Override
        public T insert(@NotNull T entity) {
            return update.insert(entity, entityType);
        }

        @Override
        public List<T> insert(@NotNull Iterable<T> entities) {
            return update.insert(entities, entityType);
        }

        @Override
        public List<T> update(@NotNull Iterable<T> entities) {
            return update.update(entities, entityType);
        }

        @Override
        public T update(@NotNull T entity) {
            return update.update(entity, entityType);
        }

        @Override
        public void delete(@NotNull Iterable<T> entities) {
            update.delete(entities, entityType);
        }

        @Override
        public void delete(@NotNull T entity) {
            update.delete(entity, entityType);
        }

        @Override
        public T updateNonNullColumn(@NotNull T entity) {
            return update.updateExcludeNullColumn(entity, entityType);
        }

        @Override
        public String toString() {
            return "Updater(" + entityType.getSimpleName() + ')';
        }
    }

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
}
