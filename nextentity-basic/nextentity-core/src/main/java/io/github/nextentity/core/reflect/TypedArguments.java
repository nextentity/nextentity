package io.github.nextentity.core.reflect;

import io.github.nextentity.core.converter.TypeConverter;
import io.github.nextentity.core.meta.EntitySchema;
import lombok.SneakyThrows;

public abstract class TypedArguments implements Arguments {

    protected final Class<?>[] types;
    protected final TypeConverter typeConverter;
    protected final EntitySchema entityType;


    public TypedArguments(Class<?>[] types, TypeConverter typeConverter, EntitySchema schema) {
        this.types = types;
        this.typeConverter = typeConverter;
        this.entityType = schema;
    }

    @SneakyThrows
    @Override
    public Object get(int index) {
        Class<?> type = types == null ? Object.class : types[index];
        Object value = getValue(index, type);
        return typeConverter == null ? value : typeConverter.convert(value, type);
    }

    protected abstract Object getValue(int index, Class<?> type);
}