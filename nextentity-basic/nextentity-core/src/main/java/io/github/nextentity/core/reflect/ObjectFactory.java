package io.github.nextentity.core.reflect;

import io.github.nextentity.core.Tuples;
import io.github.nextentity.core.api.tuple.Tuple;
import io.github.nextentity.core.exception.BeanReflectiveException;
import io.github.nextentity.core.reflect.schema.ArraySchema;
import io.github.nextentity.core.reflect.schema.Attribute;
import io.github.nextentity.core.reflect.schema.ObjectSchema;
import io.github.nextentity.core.reflect.schema.Schema;
import lombok.Data;
import lombok.experimental.Accessors;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.RecordComponent;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public interface ObjectFactory {
    Object get(Arguments arguments);

    @Data
    @Accessors(fluent = true, chain = true)
    class ObjectResult implements AttributeConstructor, ObjectSchema {

        private Class<?> type;
        private List<AttributeConstructor> attributes;
        private Attribute attribute;

        @Override
        public String name() {
            return type.getSimpleName();
        }

        @Override
        public Object get(Arguments arguments) {
            if (type.isInterface()) {
                return constructInterface(arguments);
            } else if (type.isRecord()) {
                return constructRecord(arguments);
            } else {
                return constructObject(arguments);
            }
        }

        public Object constructObject(Arguments arguments) {
            Object result = null;
            for (AttributeConstructor attr : attributes) {
                Object value = attr.get(arguments);
                if (value != null) {
                    if (result == null) {
                        result = ReflectUtil.newInstance(type);
                    }
                    attr.set(result, value);
                }
            }
            return result;
        }

        public Object constructInterface(Arguments arguments) {
            Map<Method, Object> map = new HashMap<>();
            boolean hasNonnullProperty = false;
            for (AttributeConstructor property : attributes) {
                Object extract = property.get(arguments);
                hasNonnullProperty = hasNonnullProperty || extract != null;
                map.put(property.getter(), extract);
            }
            if (hasNonnullProperty) {
                return ReflectUtil.newProxyInstance(type, map);
            } else {
                return null;
            }
        }

        public Object constructRecord(Arguments arguments) {
            Class<?>[] parameterTypes;
            RecordComponent[] components = type.getRecordComponents();
            parameterTypes = new Class[components.length];
            Map<String, Integer> index = new HashMap<>();
            for (int i = 0; i < components.length; i++) {
                RecordComponent component = components[i];
                parameterTypes[i] = component.getType();
                index.put(component.getName(), i);
            }
            try {
                Object[] args = new Object[components.length];
                boolean hasNonnullProperty = false;
                for (AttributeConstructor property : attributes) {
                    Object extract = property.get(arguments);
                    hasNonnullProperty = hasNonnullProperty || extract != null;
                    Integer i = index.get(property.name());
                    if (i != null) {
                        args[i] = extract;
                    }
                }
                if (!hasNonnullProperty) {
                    return null;
                }
                java.lang.reflect.Constructor<?> constructor = type.getDeclaredConstructor(parameterTypes);
                return constructor.newInstance(args);
            } catch (ReflectiveOperationException e) {
                throw new BeanReflectiveException(e);
            }
        }

        public void addProperty(AttributeConstructor schema) {
            if (attributes == null) {
                attributes = new ArrayList<>();
            }
            attributes.add(schema);
        }
    }

    @Data
    @Accessors(fluent = true)
    class ArrayResult implements AttributeConstructor, ArraySchema {
        private List<? extends AttributeConstructor> items;
        private Attribute attribute;

        @Override
        public String name() {
            return type().getSimpleName();
        }

        @Override
        public Class<?> type() {
            return Tuple.class;
        }

        @Override
        public Tuple get(Arguments arguments) {
            Object[] array = items().stream().map(item -> item.get(arguments)).toArray();
            return Tuples.of(array);
        }


    }

    @Data
    @Accessors(fluent = true)
    class IndexableProperty implements AttributeConstructor {

        private int index;
        private Attribute attribute;

        @Override
        public Object get(Arguments arguments) {
            return arguments.get(index);
        }

        @Override
        public Class<?> type() {
            return attribute.type();
        }

        @Override
        public String name() {
            return attribute.name();
        }

    }

    interface AttributeConstructor extends Attribute, ObjectFactory {

        Attribute attribute();

        @Override
        default String name() {
            return attribute().name();
        }

        @Override
        default Method getter() {
            return attribute().getter();
        }

        @Override
        default Method setter() {
            return attribute().setter();
        }

        @Override
        default Field field() {
            return attribute().field();
        }

        @Override
        default Schema declareBy() {
            return attribute().declareBy();
        }

        @Override
        default Class<?> type() {
            return attribute().type();
        }
    }
}
