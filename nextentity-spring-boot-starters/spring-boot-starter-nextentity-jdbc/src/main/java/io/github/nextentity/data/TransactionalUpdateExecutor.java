package io.github.nextentity.data;

import io.github.nextentity.core.UpdateExecutor;
import org.jetbrains.annotations.NotNull;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

public class TransactionalUpdateExecutor implements UpdateExecutor {

    private final UpdateExecutor target;

    public TransactionalUpdateExecutor(UpdateExecutor target) {
        this.target = target;
    }

    @Override
    @Transactional
    public <T> T insert(@NotNull T entity, @NotNull Class<T> entityType) {
        return target.insert(entity, entityType);
    }

    @Override
    @Transactional
    public <T> List<T> insert(@NotNull Iterable<T> entities, @NotNull Class<T> entityType) {
        return target.insert(entities, entityType);
    }

    @Override
    @Transactional
    public <T> List<T> update(@NotNull Iterable<T> entities, @NotNull Class<T> entityType) {
        return target.update(entities, entityType);
    }

    @Override
    @Transactional
    public <T> T update(@NotNull T entity, Class<T> entityType) {
        return target.update(entity, entityType);
    }

    @Override
    @Transactional
    public <T> void delete(@NotNull Iterable<T> entities, @NotNull Class<T> entityType) {
        target.delete(entities, entityType);
    }

    @Override
    @Transactional
    public <T> void delete(@NotNull T entity, @NotNull Class<T> entityType) {
        target.delete(entity, entityType);
    }

    @Override
    @Transactional
    public <T> T updateExcludeNullColumn(@NotNull T entity, @NotNull Class<T> entityType) {
        return target.updateExcludeNullColumn(entity, entityType);
    }

}
