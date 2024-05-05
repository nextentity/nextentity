package io.github.nextentity.data;

import io.github.nextentity.api.Repository;
import io.github.nextentity.core.RepositoryFactory;
import io.github.nextentity.core.RepositoryImpl;
import io.github.nextentity.core.TypeCastUtil;
import io.github.nextentity.core.meta.BasicAttribute;
import io.github.nextentity.core.meta.EntityType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.ResolvableType;

import java.io.Serializable;

public abstract class AbstractRepository<ID extends Serializable, T> extends RepositoryImpl<ID, T> implements Repository<ID, T> {

    public AbstractRepository() {
        super();
    }

    @Autowired
    protected void init(RepositoryFactory entitiesFactory) {
        ResolvableType type = ResolvableType.forClass(getClass())
                .as(AbstractRepository.class);
        Class<T> entityType = TypeCastUtil.cast(type.resolveGeneric(1));
        Class<?> idType = type.resolveGeneric(0);
        EntityType entity = entitiesFactory.getMetamodel().getEntity(entityType);
        BasicAttribute id = entity.id();
        Class<?> expected = id.type();
        if (expected != idType) {
            String msg = "id class defined in " + getClass() + " does not match," +
                         " expected id " + expected + ", actual id " + idType;
            throw new EntityIdTypeMismatchException(msg);
        }
        init(entitiesFactory, entityType);
    }

}