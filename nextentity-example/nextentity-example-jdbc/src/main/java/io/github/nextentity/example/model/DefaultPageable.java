package io.github.nextentity.example.model;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author HuangChengwei
 * @since 2024-03-20 8:44
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
public class DefaultPageable<T> implements Pageable<T> {

    public static final int DEFAULT_PAGE = 1;
    public static final int DEFAULT_SIZE = 10;

    private Integer page;
    private Integer size;

    public int page() {
        return Pageable.nonNullElse(page, DEFAULT_PAGE);
    }

    @Override
    public int size() {
        return Pageable.nonNullElse(size, DEFAULT_SIZE);
    }

}
