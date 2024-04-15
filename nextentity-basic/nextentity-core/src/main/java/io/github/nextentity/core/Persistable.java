package io.github.nextentity.core;

import java.io.Serializable;

/**
 * @author HuangChengwei
 * @since 2024/4/15 上午8:44
 */
public interface Persistable<ID extends Serializable> extends Serializable {
    ID getId();
}
