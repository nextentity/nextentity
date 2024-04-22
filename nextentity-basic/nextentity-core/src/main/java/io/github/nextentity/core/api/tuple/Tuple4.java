package io.github.nextentity.core.api.tuple;

public interface Tuple4<A, B, C, D> extends Tuple3<A, B, C> {

    default D get3() {
        return get(3);
    }

}
