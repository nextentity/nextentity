package io.github.nextentity.data.common;

import io.github.nextentity.core.UpdaterImpl;
import io.github.nextentity.core.api.Update;
import io.github.nextentity.core.api.Updater;
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
    public <T> T insert(T entity, Class<T> entityType) {
        return target.insert(entity, entityType);
    }

    @Override
    @Transactional
    public <T> List<T> insert(List<T> entities, Class<T> entityType) {
        return target.insert(entities, entityType);
    }

    @Override
    @Transactional
    public <T> List<T> update(List<T> entities, Class<T> entityType) {
        return target.update(entities, entityType);
    }

    @Override
    @Transactional
    public <T> T update(T entity, Class<T> entityType) {
        return target.update(entity, entityType);
    }

    @Override
    @Transactional
    public <T> void delete(Iterable<T> entities, Class<T> entityType) {
        target.delete(entities, entityType);
    }

    @Override
    @Transactional
    public <T> void delete(T entity, Class<T> entityType) {
        target.delete(entity, entityType);
    }

    @Override
    @Transactional
    public <T> T updateNonNullColumn(T entity, Class<T> entityType) {
        return target.updateNonNullColumn(entity, entityType);
    }

    @Override
    public <T> Updater<T> getUpdater(Class<T> type) {
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
