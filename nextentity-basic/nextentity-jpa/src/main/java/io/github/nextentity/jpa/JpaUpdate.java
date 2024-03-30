package io.github.nextentity.jpa;

import io.github.nextentity.core.api.Column;
import io.github.nextentity.core.api.Expression;
import io.github.nextentity.core.api.Operator;
import io.github.nextentity.core.api.Query;
import io.github.nextentity.core.api.Update;
import io.github.nextentity.core.api.Updater;
import io.github.nextentity.core.Expressions;
import io.github.nextentity.core.TypedExpressions;
import io.github.nextentity.core.UpdaterImpl;
import io.github.nextentity.core.reflect.ReflectUtil;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceUnitUtil;
import jakarta.persistence.metamodel.EntityType;
import jakarta.persistence.metamodel.SingularAttribute;
import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;

@Slf4j
public class JpaUpdate implements Update {

    private final EntityManager entityManager;
    private final Query query;
    private final PersistenceUnitUtil util;

    public JpaUpdate(EntityManager entityManager, JpaQueryExecutor jpaQueryExecutor) {
        this.entityManager = entityManager;
        this.query = jpaQueryExecutor.createQuery();
        this.util = entityManager.getEntityManagerFactory().getPersistenceUnitUtil();
    }

    @Override
    public <T> List<T> insert(List<T> entities, Class<T> entityType) {
        for (T entity : entities) {
            entityManager.persist(entity);
        }
        return entities;
    }

    @Override
    public <T> List<T> update(List<T> entities, Class<T> entityType) {
        List<Expression> ids = new ArrayList<>();
        Set<Object> uniqueValues = new HashSet<>();
        for (T entity : entities) {
            Object id = requireId(entity);
            if (uniqueValues.add(id)) {
                if (!util.isLoaded(entity)) {
                    ids.add(Expressions.of(id));
                }
            } else {
                throw new IllegalArgumentException("duplicate id");
            }
        }
        if (!ids.isEmpty()) {
            EntityType<T> entity = entityManager.getMetamodel().entity(entityType);
            SingularAttribute<? super T, ?> id = entity.getId(entity.getIdType().getJavaType());
            String name = id.getName();
            Column idPath = Expressions.column(name);
            Expression operate = Expressions.operate(idPath, Operator.IN, ids);
            List<T> dbList = query.from(entityType)
                    .where(TypedExpressions.of(operate))
                    .getList();
            if (dbList.size() != entities.size()) {
                throw new IllegalArgumentException("some id not found");
            }
        }
        List<T> list = new ArrayList<>(entities.size());
        for (T entity : entities) {
            T merge = entityManager.merge(entity);
            list.add(merge);
        }
        return list;
    }

    @Override
    public <T> void delete(Iterable<T> entities, Class<T> entityType) {
        for (T entity : entities) {
            entityManager.remove(entity);
        }
    }

    @Override
    public <T> T updateNonNullColumn(T entity, Class<T> entityType) {
        Object id = requireId(entity);
        T t = entityManager.find(entityType, id);
        if (t == null) {
            throw new IllegalArgumentException("id not found");
        }
        ReflectUtil.copyTargetNullFields(t, entity, entityType);
        return entityManager.merge(entity);
    }

    @Override
    public <T> Updater<T> getUpdater(Class<T> type) {
        return new UpdaterImpl<>(this, type);
    }

    private <T> Object requireId(T entity) {
        Object id = util.getIdentifier(entity);
        return Objects.requireNonNull(id);
    }

}
