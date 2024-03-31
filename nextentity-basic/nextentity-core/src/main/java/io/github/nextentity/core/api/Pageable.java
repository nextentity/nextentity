package io.github.nextentity.core.api;

public interface Pageable<T, R> extends Sliceable<T, R> {

    int page();

    int size();

    @Override
    default int offset() {
        return (page() - 1) * size();
    }

    @Override
    default int limit() {
        return size();
    }
}
