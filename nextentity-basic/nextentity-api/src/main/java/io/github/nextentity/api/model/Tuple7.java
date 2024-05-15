package io.github.nextentity.api.model;

public interface Tuple7<A, B, C, D, E, F, G> extends Tuple6<A, B, C, D, E, F> {
    default G get6() {
        return get(6);
    }
}
