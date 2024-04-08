package io.github.nextentity.data.common;

import io.github.nextentity.core.TypeCastUtil;
import io.github.nextentity.core.api.Query.Select;
import io.github.nextentity.core.api.Updater;
import io.github.nextentity.core.meta.Metamodel;
import org.jetbrains.annotations.Nullable;
import org.springframework.beans.factory.config.DependencyDescriptor;

import java.io.Serializable;
import java.util.Objects;

/**
 * @author huang
 * @since 2024-03-16
 */
public class AccessTypeUtil {
    public static <T> Class<T> getEntityType(DependencyDescriptor descriptor) {
        Class<?> entityType = getEntityType(new Class<?>[]{Select.class, Updater.class}, descriptor);
        Objects.requireNonNull(entityType);
        return TypeCastUtil.cast(entityType);
    }

    @Nullable
    private static Class<?> getEntityType(Class<?>[] superTypes, DependencyDescriptor descriptor) {
        for (Class<?> type : superTypes) {
            if (type.isAssignableFrom(descriptor.getDependencyType())) {
                Class<?> resolved = descriptor.getResolvableType()
                        .as(type)
                        .resolveGeneric(0);
                if (resolved != null) {
                    return resolved;
                }
            }
        }
        return null;
    }


    public static <ID extends Serializable> Class<ID> getIdType(DependencyDescriptor descriptor) {
        Class<?> type = descriptor.getResolvableType().as(ReadableAccess.class).resolveGeneric(1);
        return TypeCastUtil.cast(type);
    }

    public static <T, ID extends Serializable> void checkIdType(DependencyDescriptor descriptor,
                                                                Metamodel metamodel,
                                                                Class<T> entityType) {
        Class<ID> idType = AccessTypeUtil.getIdType(descriptor);
        Class<?> expected = metamodel.getEntity(entityType).id().javaType();
        if (expected != idType) {
            String msg = descriptor.getResolvableType() + " " + descriptor
                         + " id type mismatch, expected: " + expected + ", actual: " + idType;
            throw new EntityIdTypeMismatchException(msg);
        }
    }


    private AccessTypeUtil() {
    }
}
