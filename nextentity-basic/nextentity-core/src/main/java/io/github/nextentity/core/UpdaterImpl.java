package io.github.nextentity.core;

import io.github.nextentity.core.api.Update;
import io.github.nextentity.core.api.Updater;
import org.jetbrains.annotations.NotNull;

import java.util.List;

public class UpdaterImpl<T> implements Updater<T> {
    private final Update update;
    private final Class<T> entityType;

    public UpdaterImpl(Update update, Class<T> entityType) {
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
        return update.updateNonNullColumn(entity, entityType);
    }

    @Override
    public String toString() {
        return "UpdaterImpl{" +
               "update=" + update.getClass().getSimpleName() +
               ", entityType=" + entityType.getSimpleName() +
               '}';
    }
}
