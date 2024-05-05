package io.github.nextentity.api.model;

import java.util.List;

public interface Tuple extends Iterable<Object> {

    <T> T get(int index);

    int size();

    List<Object> toList();

    Object[] toArray();

}
