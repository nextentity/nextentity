package io.github.nextentity.core.api;

import java.util.List;

/**
 * @author HuangChengwei
 * @since 2024-04-01 9:26
 */
public interface Page<T> {
    List<T> getItems();

    long getTotal();
}
