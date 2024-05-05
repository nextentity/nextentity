package io.github.nextentity.example.model;

import io.github.nextentity.api.TypedExpression;
import io.github.nextentity.api.model.Pageable;
import io.github.nextentity.core.Pages;

/**
 * @author HuangChengwei
 * @since 2024-03-19 17:15
 */
public interface PageablePredicate<T> {

    int DEFAULT_PAGE = 1;
    int DEFAULT_SIZE = 10;

    Integer getPage();

    Integer getSize();

    TypedExpression<T, Boolean> predicate();

    default Pageable pageable() {
        return Pages.pageable(nonNullElse(getPage(), DEFAULT_PAGE), nonNullElse(getSize(), DEFAULT_SIZE));
    }

    static int nonNullElse(Integer value, int defaultValue) {
        return value != null ? value : defaultValue;
    }

}
