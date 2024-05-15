package io.github.nextentity.core.expression.impl;

import io.github.nextentity.core.expression.EntityPath;
import io.github.nextentity.core.meta.BasicAttribute;
import io.github.nextentity.core.meta.EntitySchema;
import io.github.nextentity.core.reflect.schema.Schema;
import io.github.nextentity.core.util.Iterators;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Arrays;
import java.util.Iterator;
import java.util.stream.Stream;

final class EntityPathImpl implements EntityPath, AbstractExpression {
    private final String[] paths;

    EntityPathImpl(String[] paths) {
        this.paths = paths;
    }

    @Override
    public int deep() {
        return paths.length;
    }

    @Override
    public String get(int i) {
        return paths[i];
    }

    @Override
    public EntityPath get(String path) {
        String[] strings = new String[deep() + 1];
        System.arraycopy(paths, 0, strings, 0, paths.length);
        strings[deep()] = path;
        return new EntityPathImpl(strings);
    }

    @Override
    public EntityPath parent() {
        return sub(deep() - 1);
    }

    @Override
    public EntityPath subLength(int len) {
        if (len == deep()) {
            return this;
        }
        if (len > deep()) {
            throw new IndexOutOfBoundsException();
        }
        return sub(len);
    }

    @Override
    public BasicAttribute toAttribute(EntitySchema entityType) {
        Schema type = entityType;
        for (String s : this) {
            type = ((EntitySchema) type).getAttribute(s);
        }
        return (BasicAttribute) type;
    }

    @Override
    public Stream<String> stream() {
        return Stream.of(paths);
    }

    @Nullable
    private EntityPath sub(int len) {
        if (len <= 0) {
            return null;
        }
        String[] strings = new String[len];
        System.arraycopy(paths, 0, strings, 0, strings.length);
        return new EntityPathImpl(strings);
    }

    @NotNull
    @Override
    public Iterator<String> iterator() {
        return Iterators.iterate(paths);
    }

    @Override
    public EntityPath get(EntityPath column) {
        String[] paths = new String[deep() + column.deep()];
        int i = 0;
        for (String s : this) {
            paths[i++] = s;
        }
        for (String s : column) {
            paths[i++] = s;
        }
        return new EntityPathImpl(paths);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        EntityPathImpl strings = (EntityPathImpl) o;
        return Arrays.equals(paths, strings.paths);
    }

    @Override
    public int hashCode() {
        return Arrays.hashCode(paths);
    }
}
