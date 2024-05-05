package io.github.nextentity.api;

import java.io.Serializable;

@FunctionalInterface
public interface Path<T, R> extends Serializable {

    R apply(T t);

    interface NumberPath<T, R extends Number> extends Path<T, R> {
    }

    interface BooleanPath<T> extends Path<T, Boolean> {
    }

    interface StringPath<T> extends Path<T, String> {
    }

}
