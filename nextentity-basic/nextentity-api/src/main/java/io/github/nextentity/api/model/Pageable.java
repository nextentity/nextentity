package io.github.nextentity.api.model;

public interface Pageable {

    int page();

    int size();

    default int offset() {
        return (page() - 1) * size();
    }

}
