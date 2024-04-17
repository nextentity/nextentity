package io.github.nextentity.core.meta;

import io.github.nextentity.core.exception.BeanReflectiveException;
import io.github.nextentity.core.expression.PathChain;
import io.github.nextentity.core.reflect.ReflectUtil;
import org.jetbrains.annotations.NotNull;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

public interface Attribute extends Type, PathChain {

    String name();

    Method getter();

    Method setter();

    Field field();

    Type declaringType();

    @Override
    Attribute get(String path);

    @Override
    Attribute get(PathChain column);

    @Override
    Class<?> javaType();

    @NotNull
    @Override
    default Iterator<String> iterator() {
        return referencedAttributes().stream().map(Attribute::name).iterator();
    }

    @Override
    default Attribute toAttribute(EntityType entityType) {
        return this;
    }

    @Override
    default int deep() {
        return referencedAttributes().size();
    }

    @Override
    default String get(int i) {
        return referencedAttributes().get(i).name();
    }

    @Override
    default PathChain subLength(int len) {
        return referencedAttributes().get(len - 1);
    }

    @Override
    default PathChain parent() {
        return declaringType() instanceof PathChain ? (PathChain) declaringType() : null;
    }

    static Type getDeclaringType(Type type) {
        if (type instanceof Attribute) {
            return ((Attribute) type).declaringType();
        }
        return null;
    }

    default Object get(Object entity) {
        try {
            Method getter = getter();
            if (getter != null && ReflectUtil.isAccessible(getter, entity)) {
                return getter.invoke(entity);
            } else {
                return ReflectUtil.getFieldValue(field(), entity);
            }
        } catch (IllegalAccessException | InvocationTargetException e) {
            throw new BeanReflectiveException(e);
        }
    }

    default void set(Object entity, Object value) {
        try {
            Method setter = setter();
            if (setter != null && ReflectUtil.isAccessible(setter, entity)) {
                ReflectUtil.typeCheck(value, setter.getParameterTypes()[0]);
                setter.invoke(entity, value);
            } else {
                ReflectUtil.typeCheck(value, field().getType());
                ReflectUtil.setFieldValue(field(), entity, value);
            }
        } catch (IllegalAccessException | InvocationTargetException e) {
            throw new BeanReflectiveException(e);
        }
    }

    default List<? extends Attribute> referencedAttributes() {
        Type cur = this;
        ArrayDeque<Attribute> attributes = new ArrayDeque<>(2);
        while (true) {
            if (cur instanceof Attribute) {
                // noinspection PatternVariableCanBeUsed
                Attribute attribute = (Attribute) cur;
                attributes.addFirst(attribute);
                cur = attribute.declaringType();
            } else {
                break;
            }
        }
        // noinspection Java9CollectionFactory
        return Collections.unmodifiableList(new ArrayList<>(attributes));
    }

}
