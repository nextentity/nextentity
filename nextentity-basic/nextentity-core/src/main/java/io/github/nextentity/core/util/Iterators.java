package io.github.nextentity.core.util;

import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.function.Function;
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
            return ImmutableList.ofIterable(iterable);
        }
    }

    public static <T, R> Iterable<R> map(Iterable<T> iterable, Function<? super T, ? extends R> mapper) {
        int size = sizeOf(iterable);
        if (size == 0) {
            return Collections.emptyList();
        } else if (size >= 0) {
            return new MappedSizeableIterable<>(iterable, mapper, size);
        } else {
            return new MappedIterable<>(iterable, mapper);
        }
    }


    public static <T> Object[] toArray(Iterable<T> iterable) {
        int size = Iterators.size(iterable);
        if (size == 0) {
            return EmptyArrays.OBJECT;
        }
        Object[] objects = new Object[size];
        int i = 0;
        for (T t : iterable) {
            objects[i++] = t;
        }
        return objects;
    }

    /**
     * try to get iterable size
     *
     * @param iterable iterable
     * @return size if iterable is Collection or Sizeable otherwise -1
     */
    public static int sizeOf(Iterable<?> iterable) {
        if (iterable instanceof Collection<?>) {
            return ((Collection<?>) iterable).size();
        } else if (iterable instanceof Sizeable) {
            return ((Sizeable) iterable).size();
        }
        return -1;
    }

    public static <T> Iterator<T> iterate(T[] array) {
        return new ArrayIterator<>(array);
    }

    public static <T> int size(Iterable<T> iterable) {
        int size = sizeOf(iterable);
        return size >= 0 ? size : (int) StreamSupport.stream(iterable.spliterator(), false).count();
    }

    public static final class ArrayIterator<T> implements Iterator<T> {
        int index = 0;
        private final T[] data;

        public ArrayIterator(T[] data) {
            this.data = Objects.requireNonNull(data);
        }

        public boolean hasNext() {
            return index < data.length;
        }

        public T next() {
            if (index >= data.length) {
                throw new NoSuchElementException();
            } else {
                return data[this.index++];
            }
        }
    }

    public static class MappedIterator<T, R> implements Iterator<R> {
        Iterator<T> iterator;
        Function<? super T, ? extends R> mapper;

        public MappedIterator(Iterator<T> iterator, Function<? super T, ? extends R> mapper) {
            this.iterator = iterator;
            this.mapper = mapper;
        }

        @Override
        public boolean hasNext() {
            return iterator.hasNext();
        }

        @Override
        public R next() {
            T next = iterator.next();
            return mapper.apply(next);
        }
    }

    public static class MappedSizeableIterable<T, R> extends MappedIterable<T, R> implements Sizeable {
        private final int size;

        public MappedSizeableIterable(Iterable<T> iterable, Function<? super T, ? extends R> mapper, int size) {
            super(iterable, mapper);
            this.size = size;
        }

        @Override
        public int size() {
            return size;
        }
    }

    public static class MappedIterable<T, R> implements Iterable<R> {

        private final Iterable<T> iterable;
        private final Function<? super T, ? extends R> mapper;


        public MappedIterable(Iterable<T> iterable, Function<? super T, ? extends R> mapper) {
            this.iterable = iterable;
            this.mapper = mapper;
        }

        @NotNull
        @Override
        public Iterator<R> iterator() {
            return new MappedIterator<>(iterable.iterator(), mapper);
        }

        @Override
        public String toString() {
            return StreamSupport.stream(spliterator(), false)
                    .map(String::valueOf)
                    .collect(Collectors.joining(", ", "[", "]"));
        }
    }

    public interface Sizeable {
        int size();
    }
}
