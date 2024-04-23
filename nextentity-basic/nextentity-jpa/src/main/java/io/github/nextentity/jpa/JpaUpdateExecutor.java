package io.github.nextentity.jpa;

import io.github.nextentity.core.BasicExpressions;
import io.github.nextentity.core.Expressions;
import io.github.nextentity.core.Updaters.UpdateExecutor;
import io.github.nextentity.core.api.expression.BaseExpression;
import io.github.nextentity.core.api.expression.EntityPath;
import io.github.nextentity.core.api.Operator;
import io.github.nextentity.core.api.Query;
import io.github.nextentity.core.reflect.ReflectUtil;
import io.github.nextentity.core.util.ImmutableList;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceUnitUtil;
import jakarta.persistence.metamodel.EntityType;
import jakarta.persistence.metamodel.SingularAttribute;
import lombok.extern.slf4j.Slf4j;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;

@Slf4j
public class JpaUpdateExecutor implements UpdateExecutor {

    private final EntityManager entityManager;
    private final Query query;
    private final PersistenceUnitUtil util;

    public JpaUpdateExecutor(EntityManager entityManager, JpaQueryExecutor jpaQueryExecutor) {
        this.entityManager = entityManager;
        this.query = jpaQueryExecutor.createQuery();
        this.util = entityManager.getEntityManagerFactory().getPersistenceUnitUtil();
    }

    @Override
    public <T> List<T> insert(@NotNull Iterable<T> entities, @NotNull Class<T> entityType) {
        List<T> list = ImmutableList.ofIterable(entities);
        for (T entity : entities) {
            entityManager.persist(entity);
        }
        return list;
    }

    @Override
    public <T> List<T> update(@NotNull Iterable<T> entities, @NotNull Class<T> entityType) {
        List<BaseExpression> ids = new ArrayList<>();
        Set<Object> uniqueValues = new HashSet<>();
        int size = 0;
        for (T entity : entities) {
            size++;
            Object id = requireId(entity);
            if (uniqueValues.add(id)) {
                if (!util.isLoaded(entity)) {
                    ids.add(BasicExpressions.of(id));
                }
            } else {
                throw new IllegalArgumentException("duplicate id");
            }
        }
        if (size == 0) {
            return ImmutableList.of();
        }
        if (!ids.isEmpty()) {
            EntityType<T> entity = entityManager.getMetamodel().entity(entityType);
            SingularAttribute<? super T, ?> id = entity.getId(entity.getIdType().getJavaType());
            String name = id.getName();
            EntityPath idPath = BasicExpressions.column(name);
            BaseExpression operate = BasicExpressions.operate(idPath, Operator.IN, ids);
            List<T> dbList = query.from(entityType)
                    .where(Expressions.of(operate))
                    .getList();
            if (dbList.size() != size) {
                throw new IllegalArgumentException("some id not found");
            }
        }
        List<T> list = new ArrayList<>(size);
        for (T entity : entities) {
            T merge = entityManager.merge(entity);
            list.add(merge);
        }
        return list;
    }

    @Override
    public <T> void delete(@NotNull Iterable<T> entities, @NotNull Class<T> entityType) {
        for (T entity : entities) {
            entityManager.remove(entity);
        }
    }

    @Override
    public <T> T updateExcludeNullColumn(@NotNull T entity, @NotNull Class<T> entityType) {
        Object id = requireId(entity);
        T t = entityManager.find(entityType, id);
        if (t == null) {
            throw new IllegalArgumentException("id not found");
        }
        ReflectUtil.copyTargetNullFields(t, entity, entityType);
        return entityManager.merge(entity);
    }

    private <T> Object requireId(T entity) {
        Object id = util.getIdentifier(entity);
        return Objects.requireNonNull(id);
    }

}
