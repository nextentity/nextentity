package io.github.nextentity.core.reflect;

import io.github.nextentity.core.Tuples;
import io.github.nextentity.core.exception.BeanReflectiveException;
import io.github.nextentity.core.reflect.schema.ArraySchema;
import io.github.nextentity.core.reflect.schema.Attribute;
import io.github.nextentity.core.reflect.schema.AttributeFaced;
import io.github.nextentity.core.reflect.schema.ObjectSchema;
import io.github.nextentity.core.reflect.schema.Schema;
import io.github.nextentity.core.util.tuple.Tuple;
import lombok.Data;
import lombok.experimental.Accessors;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.RecordComponent;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public interface SchemaConstructor extends Schema, AttributeFaced {
    Object construct(Arguments arguments);

    @Data
    @Accessors(fluent = true, chain = true)
    class ObjectResult implements SchemaConstructor, ObjectSchema {

        private Class<?> type;
        private List<SchemaConstructor> attributes;
        private Attribute attribute;

        @Override
        public String name() {
            return type.getSimpleName();
        }

        @Override
        public Object construct(Arguments arguments) {
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
            for (SchemaConstructor attr : attributes) {
                Object value = attr.construct(arguments);
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
            for (SchemaConstructor property : attributes) {
                Object extract = property.construct(arguments);
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
                for (SchemaConstructor property : attributes) {
                    Object extract = property.construct(arguments);
                    hasNonnullProperty = hasNonnullProperty || extract != null;
                    Integer i = index.get(property.name());
                    if (i != null) {
                        args[i] = extract;
                    }
                }
                if (!hasNonnullProperty) {
                    return null;
                }
                Constructor<?> constructor = type.getDeclaredConstructor(parameterTypes);
                return constructor.newInstance(args);
            } catch (ReflectiveOperationException e) {
                throw new BeanReflectiveException(e);
            }
        }

        public void addProperty(SchemaConstructor schema) {
            if (attributes == null) {
                attributes = new ArrayList<>();
            }
            attributes.add(schema);
        }
    }

    @Data
    @Accessors(fluent = true)
    class ArrayResult implements SchemaConstructor, ArraySchema {
        private List<? extends SchemaConstructor> items;
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
        public Tuple construct(Arguments arguments) {
            Object[] array = items().stream().map(item -> item.construct(arguments)).toArray();
            return Tuples.of(array);
        }


    }

    @Data
    @Accessors(fluent = true)
    class IndexableProperty implements SchemaConstructor {

        private int index;
        private Attribute attribute;

        @Override
        public Object construct(Arguments arguments) {
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
}
