package io.github.nextentity.core.reflect;

import io.github.nextentity.core.converter.TypeConverter;
import io.github.nextentity.core.reflect.schema.Typed;
import lombok.SneakyThrows;
import org.jetbrains.annotations.NotNull;

import java.util.Iterator;
import java.util.List;

public abstract class TypedArguments implements Arguments {

    protected final List<? extends Typed> types;
    protected final TypeConverter typeConverter;


    public TypedArguments(List<? extends Typed> types, TypeConverter typeConverter) {
        this.types = types;
        this.typeConverter = typeConverter;
    }

    @SneakyThrows
    @Override
    public Object get(int index) {
        Class<?> type = types == null ? Object.class : types.get(index).type();
        Object value = getValue(index, type);
        return typeConverter == null ? value : typeConverter.convert(value, type);
    }

    @NotNull
    @Override
    public Iterator<Object> iterator() {
        return new Itr();
    }

    protected abstract Object getValue(int index, Class<?> type);

    class Itr implements Iterator<Object> {
        int index = 0;


        @Override
        public boolean hasNext() {
            return index >= types.size();
        }

        @Override
        public Object next() {
            return get(index++);
        }
    }
}