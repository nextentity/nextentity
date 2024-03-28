package io.github.nextentity.data.common;

import io.github.nextentity.core.api.Query;
import io.github.nextentity.core.api.Update;
import io.github.nextentity.core.TypeCastUtil;
import io.github.nextentity.core.meta.Metamodel;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.ResolvableType;

public abstract class AbstractAccess<T, ID> extends AccessFacade<T, ID> implements Access<T, ID> {

    @Autowired
    protected void init(Query query, Update update, Metamodel metamodel) {
        ResolvableType type = ResolvableType.forClass(getClass())
                .as(AbstractAccess.class);
        Class<?> entityType = type.resolveGeneric(0);
        Class<?> idType = type.resolveGeneric(1);
        Class<?> expected = metamodel.getEntity(entityType).id().javaType();
        if (expected != idType) {
            String msg = "id class defined in " + getClass() + " does not match," +
                         " expected id " + expected + ", actual id " + idType;
            throw new EntityIdTypeMismatchException(msg);
        }
        init(TypeCastUtil.cast(entityType), query, update, metamodel);
    }

}