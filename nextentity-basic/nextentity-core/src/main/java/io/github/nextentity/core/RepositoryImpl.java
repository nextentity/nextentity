package io.github.nextentity.core;

import io.github.nextentity.api.Path;
import io.github.nextentity.api.Repository;
import io.github.nextentity.api.TypedExpression.OperatableExpression;
import io.github.nextentity.api.Update;
import io.github.nextentity.api.model.EntityRoot;
import io.github.nextentity.core.expression.impl.ExpressionImpls;
import io.github.nextentity.core.expression.Expressions;
import io.github.nextentity.core.meta.BasicAttribute;
import io.github.nextentity.core.util.Iterators;
import io.github.nextentity.core.util.Paths;
import lombok.experimental.Delegate;

import java.io.Serializable;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * @author HuangChengwei
 * @since 2024-04-08 15:08
 */
public class RepositoryImpl<ID extends Serializable, T> extends SelectImpl<T> implements Repository<ID, T> {

    @Delegate
    protected final EntityRoot<T> entityRoot = Paths.root();
    @Delegate
    protected Update<T> update;

    protected OperatableExpression<T, ID> idExpression;
    protected Function<T, ID> getId;

    public RepositoryImpl() {
    }

    public RepositoryImpl(RepositoryFactory repositoryFactory, Class<T> type) {
        init(repositoryFactory, type);
    }

    public RepositoryImpl(RepositoryFactory repositoryFactory, Class<T> entityType, Path<T, ID> idPath) {
        init(repositoryFactory, entityType, idPath);
    }

    private void init(RepositoryFactory entitiesFactory, Class<T> entityType, Path<T, ID> idPath) {
        super.init(entitiesFactory, entityType);
        this.update = Updaters.create(entitiesFactory.getUpdateExecutor(), entityType);
        this.idExpression = entityRoot.get(idPath);
        this.getId = idPath::apply;
    }

    protected void init(RepositoryFactory entitiesFactory, Class<T> entityType) {
        super.init(entitiesFactory, entityType);
        update = Updaters.create(entitiesFactory.getUpdateExecutor(), entityType);
        BasicAttribute idAttribute = entitiesFactory.getMetamodel().getEntity(entityType).id();
        getId = entity -> TypeCastUtil.unsafeCast(idAttribute.get(entity));
        idExpression = Expressions.of(ExpressionImpls.column(idAttribute.name()));
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
