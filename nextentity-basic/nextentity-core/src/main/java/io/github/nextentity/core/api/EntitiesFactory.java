package io.github.nextentity.core.api;

/**
 * @author HuangChengwei
 * @since 2024-04-08 11:52
 */
public interface EntitiesFactory {

    <T> Entities<T> getEntities(Class<T> entityType);

}
