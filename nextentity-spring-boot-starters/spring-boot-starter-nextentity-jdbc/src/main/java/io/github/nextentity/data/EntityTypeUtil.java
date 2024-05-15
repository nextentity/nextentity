package io.github.nextentity.data;

import io.github.nextentity.api.Repository;
import io.github.nextentity.api.Select;
import io.github.nextentity.api.Update;
import io.github.nextentity.core.TypeCastUtil;
import io.github.nextentity.core.meta.Metamodel;
import org.jetbrains.annotations.Nullable;
import org.springframework.beans.factory.config.DependencyDescriptor;

import java.io.Serializable;
import java.util.Objects;

/**
 * @author huang
 * @since 2024-03-16
 */
public class EntityTypeUtil {

    public static <T> Class<T> getEntityType(DependencyDescriptor descriptor) {
        Class<?> entityType = getEntityType(new Class<?>[]{Select.class, Update.class}, descriptor);
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


    public static <ID> Class<ID> getIdType(DependencyDescriptor descriptor) {
        Class<?> type = null;
        if (Repository.class.isAssignableFrom(descriptor.getDependencyType())) {
            type = descriptor.getResolvableType().as(Repository.class).resolveGeneric(0);
        } else if (Repository.class.isAssignableFrom(descriptor.getDependencyType())) {
            type = descriptor.getResolvableType().as(Repository.class).resolveGeneric(0);
        }
        return TypeCastUtil.cast(type);
    }

    public static <T, ID extends Serializable> void checkIdType(DependencyDescriptor descriptor,
                                                                Metamodel metamodel,
                                                                Class<T> entityType) {
        Class<ID> idType = EntityTypeUtil.getIdType(descriptor);
        Class<?> expected = metamodel.getEntity(entityType).id().type();
        if (expected != idType) {
            String msg = descriptor.getResolvableType() + " " + descriptor
                         + " id type mismatch, expected: " + expected + ", actual: " + idType;
            throw new EntityIdTypeMismatchException(msg);
        }
    }


    private EntityTypeUtil() {
    }
}
