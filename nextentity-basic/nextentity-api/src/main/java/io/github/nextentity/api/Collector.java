package io.github.nextentity.api;

import io.github.nextentity.api.model.LockModeType;
import io.github.nextentity.api.model.Page;
import io.github.nextentity.api.model.PageCollector;
import io.github.nextentity.api.model.Pageable;
import io.github.nextentity.api.model.Slice;
import io.github.nextentity.api.model.Sliceable;

import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;

/**
 * @author HuangChengwei
 * @since 2024-05-06 8:37
 */
public interface Collector<T> {

    long count();

    List<T> getList(int offset, int maxResult, LockModeType lockModeType);

    default List<T> getList(int offset, int maxResult) {
        return getList(offset, maxResult, null);
    }

    default List<T> offset(int offset) {
        return getList(offset, -1, null);
    }

    default List<T> limit(int limit) {
        return getList(0, limit, null);
    }

    boolean exist(int offset);

    default Optional<T> first() {
        return Optional.ofNullable(getFirst());
    }

    default Optional<T> first(int offset) {
        return Optional.ofNullable(getFirst(offset));
    }

    default T getFirst() {
        return getFirst(-1);
    }

    default T getFirst(int offset) {
        List<T> list = getList(offset, 1);
        return list.isEmpty() ? null : list.get(0);
    }

    default T requireSingle() {
        return Objects.requireNonNull(getSingle(-1));
    }

    default Optional<T> single() {
        return Optional.ofNullable(getSingle());
    }

    default Optional<T> single(int offset) {
        return Optional.ofNullable(getSingle(offset));
    }

    default T getSingle() {
        return getSingle(-1);
    }

    default T getSingle(int offset) {
        List<T> list = getList(offset, 2);
        if (list.size() > 1) {
            throw new IllegalStateException("found more than one");
        }
        return list.isEmpty() ? null : list.get(0);
    }

    default List<T> getList() {
        return getList(-1, -1);
    }

    default boolean exist() {
        return exist(-1);
    }

    default Optional<T> first(LockModeType lockModeType) {
        return Optional.ofNullable(getFirst(lockModeType));
    }

    default Optional<T> first(int offset, LockModeType lockModeType) {
        return Optional.ofNullable(getFirst(offset, lockModeType));
    }

    default T getFirst(LockModeType lockModeType) {
        return getFirst(-1, lockModeType);
    }

    default T getFirst(int offset, LockModeType lockModeType) {
        List<T> list = getList(offset, 1, lockModeType);
        return list.isEmpty() ? null : list.get(0);
    }

    default T requireSingle(LockModeType lockModeType) {
        return Objects.requireNonNull(getSingle(-1, lockModeType));
    }

    default Optional<T> single(LockModeType lockModeType) {
        return Optional.ofNullable(getSingle(lockModeType));
    }

    default Optional<T> single(int offset, LockModeType lockModeType) {
        return Optional.ofNullable(getSingle(offset, lockModeType));
    }

    default T getSingle(LockModeType lockModeType) {
        return getSingle(-1, lockModeType);
    }

    default T getSingle(int offset, LockModeType lockModeType) {
        List<T> list = getList(offset, 2, lockModeType);
        if (list.size() > 1) {
            throw new IllegalStateException("found more than one");
        }
        return list.isEmpty() ? null : list.get(0);
    }

    default List<T> offset(int offset, LockModeType lockModeType) {
        return getList(offset, -1, lockModeType);
    }

    default List<T> limit(int limit, LockModeType lockModeType) {
        return getList(0, limit, lockModeType);
    }

    default List<T> getList(LockModeType lockModeType) {
        return getList(-1, -1, lockModeType);
    }

    <R> R slice(Sliceable<T, R> sliceable);

    Slice<T> slice(int offset, int limit);

    <X> SubQueryBuilder<X, T> asSubQuery();

    <R> Collector<R> map(Function<? super T, ? extends R> mapper);

    Page<T> getPage(Pageable pageable);

    default <R> R getPage(PageCollector<T, R> collector) {
        return slice(collector);
    }
}
