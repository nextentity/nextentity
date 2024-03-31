package io.github.nextentity.core.domain;

import io.github.nextentity.core.api.Pageable;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * @author HuangChengwei
 * @since 2024-03-19 17:14
 */
@Data
@NoArgsConstructor
public class Page<T> {

    private List<T> items;
    private long total;

    public Page(List<T> items, long total) {
        this.items = items;
        this.total = total;
    }

    public static <T> Pageable<T, Page<T>> pageable(int page, int size) {
        return new PageableImpl<>(page, size);
    }

    @Data
    @Accessors(fluent = true)
    static class PageableImpl<T> implements Pageable<T, Page<T>> {

        private final int page;
        private final int size;
        @Override
        public Page<T> collect(List<T> list, long total) {
            return new Page<>(list, total);
        }

    }
}
