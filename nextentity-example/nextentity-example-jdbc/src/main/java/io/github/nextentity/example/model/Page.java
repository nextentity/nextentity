package io.github.nextentity.example.model;

import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * @author HuangChengwei
 * @since 2024-03-19 17:14
 */
@Data
@NoArgsConstructor
public class Page<T> {

    private List<T> content;
    private int page;
    private int size;
    private long total;

    public Page(Pageable<T> request, List<T> content, long total) {
        this.content = content;
        this.page = request.page();
        this.size = request.size();
        this.total = total;
    }
}
