package io.github.nextentity.data.common;

import io.github.nextentity.core.EntitiesFactory;
import io.github.nextentity.core.EntitiesImpl;
import io.github.nextentity.core.TypeCastUtil;
import io.github.nextentity.core.api.Entities;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.ResolvableType;

public abstract class AbstractRepository<T, ID> extends EntitiesImpl<T, ID> implements Entities<T, ID> {

    public AbstractRepository() {
        super();
    }

    @Autowired
    protected void init(EntitiesFactory entitiesFactory) {
        ResolvableType type = ResolvableType.forClass(getClass())
                .as(AbstractRepository.class);
        Class<T> entityType = TypeCastUtil.cast(type.resolveGeneric(0));
        Class<?> idType = type.resolveGeneric(1);
        Class<?> expected = entitiesFactory.getMetamodel().getEntity(entityType).id().javaType();
        if (expected != idType) {
            String msg = "id class defined in " + getClass() + " does not match," +
                         " expected id " + expected + ", actual id " + idType;
            throw new EntityIdTypeMismatchException(msg);
        }
        init(entitiesFactory, entityType);
    }

}