package io.github.nextentity.jpa;

import io.github.nextentity.api.Expression;
import io.github.nextentity.core.QueryConfig;
import io.github.nextentity.core.SelectImpl;
import io.github.nextentity.core.UpdateExecutor;
import io.github.nextentity.core.expression.EntityPath;
import io.github.nextentity.core.expression.Operator;
import io.github.nextentity.core.expression.impl.ExpressionImpls;
import io.github.nextentity.core.expression.Expressions;
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
    private final QueryConfig queryConfig;
    private final PersistenceUnitUtil util;

    public JpaUpdateExecutor(EntityManager entityManager, QueryConfig queryConfig) {
        this.entityManager = entityManager;
        this.queryConfig = queryConfig;
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
        List<Expression> ids = new ArrayList<>();
        Set<Object> uniqueValues = new HashSet<>();
        int size = 0;
        for (T entity : entities) {
            size++;
            Object id = requireId(entity);
            if (uniqueValues.add(id)) {
                if (!util.isLoaded(entity)) {
                    ids.add(ExpressionImpls.of(id));
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
            EntityPath idPath = ExpressionImpls.column(name);
            Expression operate = ExpressionImpls.operate(idPath, Operator.IN, ids);
            List<T> dbList = new SelectImpl<>(queryConfig, entityType)
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
