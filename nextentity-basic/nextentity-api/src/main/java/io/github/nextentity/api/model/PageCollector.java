package io.github.nextentity.api.model;

import java.util.List;

/**
 * @author HuangChengwei
 * @since 2024/4/24 下午1:53
 */
public interface PageCollector<T, R> extends Pageable, Sliceable<T, R> {

    R collect(List<T> list, long total);

    @Override
    default int offset() {
        return Pageable.super.offset();
    }
}
