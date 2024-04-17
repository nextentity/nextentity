package io.github.nextentity.core.reflect;

import io.github.nextentity.core.Tuples;
import io.github.nextentity.core.converter.TypeConverter;
import io.github.nextentity.core.expression.SelectElement;
import io.github.nextentity.core.expression.SelectEntity;
import io.github.nextentity.core.expression.SelectExpression;
import io.github.nextentity.core.expression.Selected;
import io.github.nextentity.core.expression.MultiSelected;
import io.github.nextentity.core.expression.SingleSelected;
import io.github.nextentity.core.meta.Attribute;
import io.github.nextentity.core.meta.Metamodel;
import lombok.SneakyThrows;

import java.util.List;

public abstract class ResultExtractor implements Arguments {

    protected int offset = 0;
    protected final Class<?>[] types;
    protected final TypeConverter typeConverter;
    protected final Metamodel metamodel;
    protected final Class<?> entityType;


    public ResultExtractor(Class<?>[] types, TypeConverter typeConverter, Metamodel metamodel, Class<?> entityType) {
        this.types = types;
        this.typeConverter = typeConverter;
        this.metamodel = metamodel;
        this.entityType = entityType;
    }

    @SneakyThrows
    @Override
    public Object get(int index) {
        index += offset;
        Class<?> type = types[index];
        Object value = getValue(index, type);
        return typeConverter.convert(value, type);
    }

    protected abstract Object getValue(int index, Class<?> type);

    public Object next() {
        Object result = get(0);
        addOffset(1);
        return result;
    }

    public void addOffset(int offset) {
        this.offset += offset;
    }

    public Metamodel metamodel() {
        return metamodel;
    }


    public Object extractRow(Selected select) {
        List<? extends SelectElement> elements = select.elements();
        Object row;
        if (select instanceof MultiSelected) {
            Object[] array = elements.stream()
                    .map(this::extractValue)
                    .toArray();
            row = Tuples.of(array);
        } else if (select instanceof SingleSelected) {
            row = extractValue(elements.get(0));
        } else {
            throw new IllegalArgumentException();
        }
        return row;
    }

    public Object extractValue(SelectElement element) {
        if (element instanceof SelectExpression) {
            return next();
        } else if (element instanceof SelectEntity) {
            SelectEntity selectEntity = (SelectEntity) element;
            Class<?> resultType = selectEntity.javaType();
            List<? extends Attribute> attributes = selectEntity.attributes();
            InstanceConstructor extractor = ReflectUtil.getRowInstanceConstructor(attributes, resultType);
            Object result = extractor.newInstance(this);
            addOffset(attributes.size());
            return result;
        } else {
            throw new IllegalStateException();
        }
    }
}