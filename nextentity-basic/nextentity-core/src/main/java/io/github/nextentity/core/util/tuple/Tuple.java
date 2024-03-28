package io.github.nextentity.core.util.tuple;

import java.util.List;

public interface Tuple extends Iterable<Object> {

    <T> T get(int index);

    int size();

    List<Object> toList();

    Object[] toArray();

}
