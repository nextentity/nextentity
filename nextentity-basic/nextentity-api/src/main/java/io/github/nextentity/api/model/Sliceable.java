package io.github.nextentity.api.model;

import java.util.List;

public interface Sliceable<T, U> {

    int offset();

    int limit();

    U collect(List<T> list, long total);

}
