package io.github.nextentity.core.api;

import java.io.Serializable;

@FunctionalInterface
public interface Path<T, R> extends Serializable {

    R apply(T t);

    interface NumberPath<T, R extends Number & Comparable<R>> extends ComparablePath<T, R> {
    }

    interface ComparablePath<T, R extends Comparable<R>> extends Path<T, R> {
    }

    interface BooleanPath<T> extends ComparablePath<T, Boolean> {
    }

    interface StringPath<T> extends ComparablePath<T, String> {
    }

}
