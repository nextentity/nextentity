package io.github.nextentity.example.model;

import io.github.nextentity.core.api.Sliceable;

import java.util.List;

/**
 * @author HuangChengwei
 * @since 2024-03-20 8:44
 */
public interface Pageable<T> extends Sliceable<T, Page<T>> {

    static <T> Pageable<T> of(int page, int size) {
        return new DefaultPageable<>(page, size);
    }

    int page();

    int size();

    @Override
    default int offset() {
        return (page() - 1) * limit();
    }

    @Override
    default int limit() {
        return size();
    }

    @Override
    default Page<T> collect(List<T> list, long total) {
        return new Page<>(this, list, total);
    }

    static int nonNullElse(Integer value, int defaultValue) {
        return value != null ? value : defaultValue;
    }
}
