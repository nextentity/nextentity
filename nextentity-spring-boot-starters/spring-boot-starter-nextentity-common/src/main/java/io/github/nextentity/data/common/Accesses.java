package io.github.nextentity.data.common;

import io.github.nextentity.core.api.Query;
import io.github.nextentity.core.api.Update;
import io.github.nextentity.core.meta.Metamodel;
import org.springframework.beans.factory.config.DependencyDescriptor;

import java.io.Serializable;

/**
 * @author HuangChengwei
 * @since 2024-03-27 12:33
 */
public class Accesses {

    public static <T, ID> Access<T, ID> of(Class<T> entityType, Query query, Update update, Metamodel metamodel) {
        return new AccessImpl<>(entityType, query, update, metamodel);
    }

    public static <T, ID extends Serializable> Access<T, ID> of(DependencyDescriptor descriptor, Query query, Update update, Metamodel metamodel) {
        Class<T> entityType = AccessTypeUtil.getEntityType(descriptor);
        Class<?> dependencyType = descriptor.getDependencyType();
        if (Access.class.isAssignableFrom(dependencyType)) {
            AccessTypeUtil.checkIdType(descriptor, metamodel, entityType);
        }
        return of(entityType, query, update, metamodel);
    }

    static class AccessImpl<T, ID> extends AccessFacade<T, ID> implements Access<T, ID> {
        public AccessImpl(Class<T> entityType, Query query, Update update, Metamodel metamodel) {
            init(entityType, query, update, metamodel);
        }
    }


}
