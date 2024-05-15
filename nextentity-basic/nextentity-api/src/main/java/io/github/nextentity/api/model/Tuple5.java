package io.github.nextentity.api.model;

public interface Tuple5<A, B, C, D, E> extends Tuple4<A, B, C, D> {

    default E get4() {
        return get(4);
    }

}
