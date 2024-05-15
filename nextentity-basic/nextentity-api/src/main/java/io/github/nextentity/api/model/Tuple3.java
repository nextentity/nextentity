package io.github.nextentity.api.model;

public interface Tuple3<A, B, C> extends Tuple2<A, B> {
    default C get2() {
        return get(2);
    }
}
