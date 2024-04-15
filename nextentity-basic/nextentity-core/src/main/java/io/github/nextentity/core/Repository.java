package io.github.nextentity.core;

import io.github.nextentity.core.api.EntityRoot;
import io.github.nextentity.core.api.Query.Select;
import io.github.nextentity.core.api.Update;

import java.io.Serializable;
import java.util.List;
import java.util.Map;

/**
 * @author HuangChengwei
 * @since 2024/4/15 上午8:55
 */
public interface Repository<ID extends Serializable, T extends Persistable<ID>>
        extends Select<T>, Update<T>, EntityRoot<T> {

    T get(ID id);

    List<T> getAll(Iterable<? extends ID> ids);

    Map<ID, T> getMap(Iterable<? extends ID> ids);

}
