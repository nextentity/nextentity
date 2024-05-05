package io.github.nextentity.api;

import org.jetbrains.annotations.NotNull;

import java.util.List;

public interface Update<T> {

    T insert(@NotNull T entity);

    List<T> insert(@NotNull Iterable<T> entities);

    List<T> update(@NotNull Iterable<T> entities);

    T update(@NotNull T entity);

    void delete(@NotNull Iterable<T> entities);

    void delete(@NotNull T entity);

    T updateNonNullColumn(@NotNull T entity);

}
