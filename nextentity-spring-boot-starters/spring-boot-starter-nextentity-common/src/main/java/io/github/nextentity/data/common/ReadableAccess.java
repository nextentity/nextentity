package io.github.nextentity.data.common;

import io.github.nextentity.core.api.Query.Select;

import java.util.List;
import java.util.Map;

/**
 * @author HuangChengwei
 * @since 2024-03-27 13:09
 */
public interface ReadableAccess<T, ID> extends Select<T> {

    T get(ID id);

    List<T> getAll(Iterable<? extends ID> ids);

    Map<ID, T> getMap(Iterable<? extends ID> ids);

}
