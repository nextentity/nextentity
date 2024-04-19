package io.github.nextentity.core;

import io.github.nextentity.core.api.Entities;
import io.github.nextentity.core.api.EntityRoot;
import io.github.nextentity.core.api.Expression.OperatableExpression;
import io.github.nextentity.core.api.Path;
import io.github.nextentity.core.api.Update;
import io.github.nextentity.core.meta.BasicAttribute;
import io.github.nextentity.core.util.Iterators;
import io.github.nextentity.core.util.Paths;
import lombok.experimental.Delegate;

import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * @author HuangChengwei
 * @since 2024-04-08 15:08
 */
public class EntitiesImpl<T, ID> extends SelectImpl<T> implements Entities<ID, T> {

    @Delegate
    protected final EntityRoot<T> entityRoot = Paths.root();
    @Delegate
    protected Update<T> update;

    protected OperatableExpression<T, ID> idExpression;
    protected Function<T, ID> getId;

    public EntitiesImpl(EntitiesFactory entitiesFactory, Class<T> entityType, Path<T, ID> idPath) {
        init(entitiesFactory, entityType, idPath);
    }


    public EntitiesImpl(EntitiesFactory entitiesFactory, Class<T> type) {
        init(entitiesFactory, type);
    }

    protected EntitiesImpl() {
    }

    private void init(EntitiesFactory entitiesFactory, Class<T> entityType, Path<T, ID> idPath) {
        super.init(entityType, entitiesFactory.getQueryExecutor(), entitiesFactory.getQueryPostProcessor());
        this.update = Updaters.create(entitiesFactory.getUpdateExecutor(), entityType);
        this.idExpression = entityRoot.get(idPath);
        this.getId = idPath::apply;
    }

    protected void init(EntitiesFactory entitiesFactory, Class<T> entityType) {
        super.init(entityType, entitiesFactory.getQueryExecutor(), entitiesFactory.getQueryPostProcessor());
        update = Updaters.create(entitiesFactory.getUpdateExecutor(), entityType);
        BasicAttribute idAttribute = entitiesFactory.getMetamodel().getEntity(entityType).id();
        getId = entity -> TypeCastUtil.unsafeCast(idAttribute.get(entity));
        idExpression = Expressions.of(BasicExpressions.column(idAttribute.name()));
    }

    public T get(ID id) {
        return where(this.idExpression.eq(id)).getSingle();
    }

    public List<T> getAll(Iterable<? extends ID> ids) {
        return where(this.idExpression.in(Iterators.toList(ids))).getList();
    }

    public Map<ID, T> getMap(Iterable<? extends ID> ids) {
        return getAll(ids).stream().collect(Collectors.toMap(getId, Function.identity()));
    }

}
