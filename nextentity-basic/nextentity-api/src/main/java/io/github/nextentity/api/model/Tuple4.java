package io.github.nextentity.api.model;

public interface Tuple4<A, B, C, D> extends Tuple3<A, B, C> {

    default D get3() {
        return get(3);
    }

}
