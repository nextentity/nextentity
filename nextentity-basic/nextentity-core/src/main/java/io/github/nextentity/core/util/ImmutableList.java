package io.github.nextentity.core.util;


import io.github.nextentity.core.TypeCastUtil;
import org.jetbrains.annotations.NotNull;

import java.io.Serializable;
import java.util.AbstractList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.RandomAccess;
import java.util.function.Predicate;
import java.util.function.UnaryOperator;
import java.util.stream.Collector;

/**
 * @author HuangChengwei
 * @since 2024/4/23 上午8:50
 */
public class ImmutableList<E> extends AbstractList<E> implements List<E>, RandomAccess, Cloneable, Serializable {

    private static final ImmutableList<?> EMPTY = new ImmutableList<>(EmptyArrays.OBJECT);

    private final Object[] elements;

    @SafeVarargs
    public static <T> ImmutableList<T> of(T... elements) {
        if (elements.length == 0) {
            return empty();
        }
        return new ImmutableList<>(elements);
    }

    public static <T> ImmutableList<T> ofIterable(Iterable<T> iterable) {
        return iterable instanceof Collection ? ofCollection((Collection<T>) iterable) : new ImmutableList<T>(Iterators.toArray(iterable));
    }

    private static <T> @NotNull ImmutableList<T> ofCollection(Collection<T> collection) {
        if (collection instanceof ImmutableList) {
            return (ImmutableList<T>) collection;
        } else if (collection.isEmpty()) {
            return empty();
        }
        return new ImmutableList<>(collection);
    }

    public static <T> ImmutableList<T> concat(Collection<? extends T> a, Collection<? extends T> b) {
        Builder<T> list = new Builder<>(a.size() + b.size());
        list.addAll(a);
        list.addAll(b);
        return list.build();
    }

    public static <T> @NotNull ImmutableList<T> empty() {
        return TypeCastUtil.unsafeCast(EMPTY);
    }

    public ImmutableList(Collection<E> collection) {
        this(collection.toArray());
    }

    private ImmutableList(Object[] elements) {
        this.elements = elements;
    }

    @Override
    public int size() {
        return elements.length;
    }

    @NotNull
    @Override
    public Iterator<E> iterator() {
        return new Itr();
    }

    @Override
    public ListIterator<E> listIterator() {
        return new Itr();
    }

    private class Itr implements ListIterator<E> {
        int cursor;

        @Override
        public boolean hasNext() {
            return cursor < elements.length;
        }

        @Override
        public E next() {
            return get(cursor++);
        }

        @Override
        public boolean hasPrevious() {
            return cursor > 0;
        }

        @Override
        public E previous() {
            return get(--cursor);
        }

        @Override
        public int nextIndex() {
            return cursor;
        }

        @Override
        public int previousIndex() {
            return cursor - 1;
        }

        @Override
        public void remove() {
            throw new UnsupportedOperationException();
        }

        @Override
        public void set(E e) {
            throw new UnsupportedOperationException();
        }

        @Override
        public void add(E e) {
            throw new UnsupportedOperationException();
        }
    }

    @NotNull
    @Override
    public Object[] toArray() {
        return elements.clone();
    }

    @NotNull
    @Override
    public <T> T[] toArray(T[] a) {
        int size = size();
        if (a.length < size) {
            return TypeCastUtil.unsafeCast(Arrays.copyOf(elements, size, a.getClass()));
        }
        //noinspection SuspiciousSystemArraycopy
        System.arraycopy(elements, 0, a, 0, size);
        if (a.length > size) {
            a[size] = null;
        }
        return a;
    }

    @Override
    public boolean add(E t) {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean remove(Object o) {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean addAll(@NotNull Collection<? extends E> c) {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean addAll(int index, @NotNull Collection<? extends E> c) {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean removeAll(@NotNull Collection<?> c) {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean retainAll(@NotNull Collection<?> c) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void clear() {
        throw new UnsupportedOperationException();
    }

    @Override
    public E get(int index) {
        return TypeCastUtil.unsafeCast(elements[index]);
    }

    @Override
    public E set(int index, E element) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void add(int index, E element) {
        throw new UnsupportedOperationException();
    }

    @Override
    public E remove(int index) {
        throw new UnsupportedOperationException();
    }


    @Override
    public void replaceAll(UnaryOperator<E> operator) {
        throw new UnsupportedOperationException();
    }

    @Override
    public void sort(Comparator<? super E> c) {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean removeIf(Predicate<? super E> filter) {
        throw new UnsupportedOperationException();
    }

    @Override
    public ImmutableList<E> clone() {
        try {
            return TypeCastUtil.unsafeCast(super.clone());
        } catch (CloneNotSupportedException e) {
            throw new InternalError(e);
        }
    }

    @NotNull
    @Override
    public ImmutableList<E> subList(int fromIndex, int toIndex) {
        subListRangeCheck(fromIndex, toIndex, size());
        int newSize = toIndex - fromIndex;
        if (newSize == 0) {
            return empty();
        } else if (size() == newSize) {
            return this;
        }
        Object[] objects = new Object[newSize];
        System.arraycopy(elements, fromIndex, objects, 0, newSize);
        return new ImmutableList<>(objects);
    }

    static void subListRangeCheck(int fromIndex, int toIndex, int size) {
        if (fromIndex < 0)
            throw new IndexOutOfBoundsException("fromIndex = " + fromIndex);
        if (toIndex > size)
            throw new IndexOutOfBoundsException("toIndex = " + toIndex);
        if (fromIndex > toIndex)
            throw new IllegalArgumentException("fromIndex(" + fromIndex + ") > toIndex(" + toIndex + ")");
    }

    public static <T> Collector<T, ?, ImmutableList<T>> collector() {
        return Collector.of(
                Builder<T>::new, Builder::add,
                Builder::addAll, Builder::build);
    }

    public static <T> @NotNull Collector<T, ?, ImmutableList<T>> collector(int initialCapacity) {
        return Collector.of(
                () -> new Builder<T>(initialCapacity), Builder::add,
                Builder::addAll, Builder::build);
    }

    public static class Builder<E> {
        public static final int DEFAULT_INITIAL_CAPACITY = 8;
        public static final int MAX_LENGTH = Integer.MAX_VALUE - 8;
        public static final int HALF_MAX_LENGTH = MAX_LENGTH >> 1;

        Object[] elements;
        int size;

        public Builder(int initialCapacity) {
            this.elements = initialCapacity <= 0 ? EmptyArrays.OBJECT : new Object[initialCapacity];
        }

        public Builder() {
            this.elements = EmptyArrays.OBJECT;
        }

        public void add(Object o) {
            ensureCapacity(size + 1);
            elements[size++] = o;
        }

        public Builder<E> addAll(Builder<? extends E> builder) {
            ensureCapacity(size + builder.elements.length);
            System.arraycopy(builder.elements, 0, elements, size, builder.size);
            size += builder.size;
            return this;
        }


        void ensureCapacity(int minCapacity) {
            if (elements.length >= minCapacity) {
                return;
            }
            int newCapacity = Math.max(elements.length, DEFAULT_INITIAL_CAPACITY);
            while (newCapacity < minCapacity) {
                if (newCapacity >= HALF_MAX_LENGTH) {
                    newCapacity = MAX_LENGTH;
                    break;
                } else {
                    newCapacity <<= 1;
                }
            }
            elements = Arrays.copyOf(elements, newCapacity);
        }


        public void addAll(Collection<? extends E> c) {
            if (c.isEmpty()) {
                return;
            }
            ensureCapacity(size + c.size());
            for (E e : c) {
                elements[size++] = e;
            }
        }

        public ImmutableList<E> build() {
            if (elements.length == size) {
                return new ImmutableList<>(elements);
            } else if (size == 0) {
                return empty();
            } else {
                Object[] element = new Object[size];
                System.arraycopy(elements, 0, element, 0, size);
                return new ImmutableList<>(element);
            }
        }

        public boolean isEmpty() {
            return size == 0;
        }
    }

}
