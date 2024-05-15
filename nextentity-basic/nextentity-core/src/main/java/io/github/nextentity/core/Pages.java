package io.github.nextentity.core;

import io.github.nextentity.api.model.Page;
import io.github.nextentity.api.model.Pageable;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * @author HuangChengwei
 * @since 2024-04-01 9:30
 */
public class Pages {

    public static <T> Page<T> page(List<T> items, long total) {
        return new PageImpl<>(items, total);
    }

    public static Pageable pageable(int page, int size) {
        return new PageableImpl(page, size);
    }

    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    public static class PageImpl<T> implements Page<T> {
        private List<T> items;
        private long total;
    }

    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    @Accessors(fluent = true)
    public static class PageableImpl implements Pageable {
        private int page, size;
    }

}
