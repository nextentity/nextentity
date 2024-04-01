package io.github.nextentity.core.api;

public interface Pageable {

    int page();

    int size();

    default int offset() {
        return (page() - 1) * size();
    }

}
