package io.github.nextentity.core;

import io.github.nextentity.api.Update;
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
        private final UpdateExecutor updateExecutor;
        private final Class<T> entityType;

        public UpdateImpl(UpdateExecutor updateExecutor, Class<T> entityType) {
            this.entityType = entityType;
            this.updateExecutor = updateExecutor;
        }

        @Override
        public T insert(@NotNull T entity) {
            return updateExecutor.insert(entity, entityType);
        }

        @Override
        public List<T> insert(@NotNull Iterable<T> entities) {
            return updateExecutor.insert(entities, entityType);
        }

        @Override
        public List<T> update(@NotNull Iterable<T> entities) {
            return updateExecutor.update(entities, entityType);
        }

        @Override
        public T update(@NotNull T entity) {
            return updateExecutor.update(entity, entityType);
        }

        @Override
        public void delete(@NotNull Iterable<T> entities) {
            updateExecutor.delete(entities, entityType);
        }

        @Override
        public void delete(@NotNull T entity) {
            updateExecutor.delete(entity, entityType);
        }

        @Override
        public T updateNonNullColumn(@NotNull T entity) {
            return updateExecutor.updateExcludeNullColumn(entity, entityType);
        }

        @Override
        public String toString() {
            return "Updater(" + entityType.getSimpleName() + ')';
        }
    }

}
