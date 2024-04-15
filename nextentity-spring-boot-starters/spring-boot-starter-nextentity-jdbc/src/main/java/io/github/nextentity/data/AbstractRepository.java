package io.github.nextentity.data;

import io.github.nextentity.core.Persistable;
import io.github.nextentity.core.Repository;

import java.io.Serializable;

public abstract class AbstractRepository<ID extends Serializable, T extends Persistable<ID>>
        extends AbstractEntities<T, ID>
        implements Repository<ID, T> {

}