package io.github.nextentity.data.common;

import io.github.nextentity.core.api.Update;
import io.github.nextentity.core.api.Updater;
import io.github.nextentity.core.UpdaterImpl;
import org.jetbrains.annotations.NotNull;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

public class TransactionalUpdate implements Update, ApplicationContextAware {

    private final Update target;
    private ApplicationContext applicationContext;
    private Update update;

    public TransactionalUpdate(Update target) {
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
    public <T> T updateNonNullColumn(@NotNull T entity, @NotNull Class<T> entityType) {
        return target.updateNonNullColumn(entity, entityType);
    }

    @Override
    public <T> Updater<T> getUpdater(@NotNull Class<T> type) {
        return new UpdaterImpl<>(getUpdate(), type);
    }

    @NotNull
    private Update getUpdate() {
        if (update != null) {
            return update;
        }
        return update = applicationContext.getBean(TransactionalUpdate.class);
    }

    @Override
    public void setApplicationContext(@NotNull ApplicationContext applicationContext) throws BeansException {
        this.applicationContext = applicationContext;
    }
}
