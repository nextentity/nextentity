package io.github.nextentity.core.api;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.function.Predicate;
import java.util.function.UnaryOperator;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

public interface Lists {

    static <E> List<E> of(E e1, E e2) {
        return List.of(e1, e2);
    }

    static <E> List<E> of(E e1, E e2, E e3) {
        return List.of(e1, e2, e3);
    }

    static <E> List<E> of(E e1, E e2, E e3, E e4) {
        return List.of(e1, e2, e3, e4);
    }

    static <E> List<E> of(E e1, E e2, E e3, E e4, E e5) {
        return List.of(e1, e2, e3, e4, e5);
    }

    static <E> List<E> of(E e1, E e2, E e3, E e4, E e5, E e6) {
        return List.of(e1, e2, e3, e4, e5, e6);
    }

    static <E> List<E> of(E e1, E e2, E e3, E e4, E e5, E e6, E e7) {
        return List.of(e1, e2, e3, e4, e5, e6, e7);
    }

    static <E> List<E> of(E e1, E e2, E e3, E e4, E e5, E e6, E e7, E e8) {
        return List.of(e1, e2, e3, e4, e5, e6, e7, e8);
    }

    static <E> List<E> of(E e1, E e2, E e3, E e4, E e5, E e6, E e7, E e8, E e9) {
        return List.of(e1, e2, e3, e4, e5, e6, e7, e8, e9);
    }

    static <E> List<E> of(E e1, E e2, E e3, E e4, E e5, E e6, E e7, E e8, E e9, E e10) {
        return List.of(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10);
    }

    @SafeVarargs
    static <E> List<E> of(E... es) {
        return List.of(es);
    }

    static <E> List<E> of(E e) {
        return List.of(e);
    }

    static <E> List<E> of() {
        return List.of();
    }

    static <T> Stream<T> iterate(T seed, Predicate<? super T> hasNext, UnaryOperator<T> next) {
        return Stream.iterate(seed, hasNext, next);
    }

    static <T> List<T> concat(Collection<? extends T> collection, Collection<? extends T> value) {
        ArrayList<T> list = new ArrayList<>(collection.size() + 1);
        list.addAll(collection);
        list.addAll(value);
        return list;
    }

    static <T> List<T> toArrayList(Iterable<T> iterable) {
        if (iterable.getClass() == ArrayList.class) {
            return (List<T>) iterable;
        } else if (iterable instanceof Collection) {
            return new ArrayList<>((Collection<? extends T>) iterable);
        } else {
            return StreamSupport.stream(iterable.spliterator(), false).collect(Collectors.toList());
        }
    }

}
