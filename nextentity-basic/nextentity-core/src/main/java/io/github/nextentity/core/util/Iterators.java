package io.github.nextentity.core.util;

import io.github.nextentity.core.ArrayIterator;

import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

/**
 * @author HuangChengwei
 * @since 2024-04-08 8:50
 */
public class Iterators {

    public static <T> List<T> toList(Iterable<T> iterable) {
        if (iterable instanceof List<T>) {
            return (List<T>) iterable;
        } else {
            return StreamSupport
                    .stream(iterable.spliterator(), false)
                    .collect(Collectors.toList());
        }
    }

    public static <T> Iterator<T> iterate(T[] array) {
        return new ArrayIterator<>(array);
    }

}
