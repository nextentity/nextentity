package io.github.nextentity.core.api;

import io.github.nextentity.core.api.Query.Select;

import java.util.List;
import java.util.Map;

/**
 * @author HuangChengwei
 * @since 2024-04-08 11:47
 */
public interface Entities<ID, T> extends Select<T>, Update<T>, EntityRoot<T> {

    T get(ID id);

    List<T> getALl(Iterable<? extends ID> ids);

    Map<ID, T> getMap(Iterable<? extends ID> ids);

}
