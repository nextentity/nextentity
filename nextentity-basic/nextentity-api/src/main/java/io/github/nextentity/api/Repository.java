package io.github.nextentity.api;

import io.github.nextentity.api.model.EntityRoot;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

/**
 * @author HuangChengwei
 * @since 2024/4/15 上午8:55
 */
public interface Repository<ID extends Serializable, T>
        extends Select<T>, Update<T>, EntityRoot<T> {

    T get(ID id);

    List<T> getAll(Iterable<? extends ID> ids);

    Map<ID, T> getMap(Iterable<? extends ID> ids);

}
