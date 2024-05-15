package io.github.nextentity.api;

import java.util.Collection;
import java.util.List;

/**
 * @author HuangChengwei
 * @since 2024-05-06 8:38
 */
public
interface SelectGroupByStep<T, U> extends SelectOrderByStep<T, U> {
    SelectHavingStep<T, U> groupBy(List<? extends TypedExpression<T, ?>> expressions);

    SelectHavingStep<T, U> groupBy(Path<T, ?> path);

    SelectHavingStep<T, U> groupBy(Collection<Path<T, ?>> paths);

    default SelectHavingStep<T, U> groupBy(Path<T, ?> p0, Path<T, ?> p1) {
        return groupBy(List.of(p0, p1));
    }

    default SelectHavingStep<T, U> groupBy(Path<T, ?> p0, Path<T, ?> p1, Path<T, ?> p2) {
        return groupBy(List.of(p0, p1, p2));
    }

    default SelectHavingStep<T, U> groupBy(Path<T, ?> p0, Path<T, ?> p1, Path<T, ?> p2, Path<T, ?> p3) {
        return groupBy(List.of(p0, p1, p2, p3));
    }

    default SelectHavingStep<T, U> groupBy(Path<T, ?> p0, Path<T, ?> p1, Path<T, ?> p2, Path<T, ?> p3, Path<T, ?> p4) {
        return groupBy(List.of(p0, p1, p2, p3, p4));
    }

    default SelectHavingStep<T, U> groupBy(Path<T, ?> p0, Path<T, ?> p1, Path<T, ?> p2, Path<T, ?> p3, Path<T, ?> p4, Path<T, ?> p5) {
        return groupBy(List.of(p0, p1, p2, p3, p4, p5));
    }
}
