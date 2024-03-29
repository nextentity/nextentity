package io.github.nextentity.core.api;

non-sealed public interface Column extends Expression, Iterable<String> {
    int size();

    String get(int i);

    Column get(String path);

    Column get(Column column);

    Column parent();

    Column subLength(int len);
}
