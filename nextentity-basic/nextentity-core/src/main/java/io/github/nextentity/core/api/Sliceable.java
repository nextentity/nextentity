package io.github.nextentity.core.api;

import java.util.List;

public interface Sliceable<T, U> {

    int offset();

    int limit();

    U collect(List<T> list, long total);

}
