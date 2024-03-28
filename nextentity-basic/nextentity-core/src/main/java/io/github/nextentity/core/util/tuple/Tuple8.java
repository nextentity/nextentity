package io.github.nextentity.core.util.tuple;

public interface Tuple8<A, B, C, D, E, F, G, H> extends Tuple7<A, B, C, D, E, F, G> {
    default H get7() {
        return get(7);
    }
}
