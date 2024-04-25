package io.github.nextentity.core.reflect;

import io.github.nextentity.core.Tuples;
import io.github.nextentity.core.api.tuple.Tuple;
import io.github.nextentity.core.exception.BeanReflectiveException;
import io.github.nextentity.core.meta.BasicAttribute;
import io.github.nextentity.core.meta.ProjectionBasicAttribute;
import io.github.nextentity.core.reflect.schema.ArraySchema;
import io.github.nextentity.core.reflect.schema.Attribute;
import io.github.nextentity.core.reflect.schema.ObjectSchema;
import io.github.nextentity.core.reflect.schema.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.RecordComponent;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public interface ObjectFactory extends Schema {
    Object get(Arguments arguments);

    @Data
    @EqualsAndHashCode(callSuper = true)
    @Accessors(fluent = true, chain = true)
    class ObjectAttribute extends ObjectResult implements ObjectAttributeFaced {
        private Attribute attribute;
    }

    @Data
    @Accessors(fluent = true, chain = true)
    class ObjectResult implements ObjectFactory, ObjectSchema {

        private Class<?> type;
        private List<ObjectAttributeFaced> attributes;

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
            for (ObjectAttributeFaced attr : attributes) {
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
            boolean notNull = false;
            for (ObjectAttributeFaced attribute : attributes) {
                Object value = attribute.get(arguments);
                notNull = notNull || value != null;
                map.put(attribute.getter(), value);
            }
            if (notNull) {
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
                boolean notNull = false;
                for (ObjectAttributeFaced attribute : attributes) {
                    Object arg = attribute.get(arguments);
                    notNull = notNull || arg != null;
                    Integer i = index.get(attribute.name());
                    if (i != null) {
                        args[i] = arg;
                    }
                }
                if (!notNull) {
                    return null;
                }
                java.lang.reflect.Constructor<?> constructor = type.getDeclaredConstructor(parameterTypes);
                return constructor.newInstance(args);
            } catch (ReflectiveOperationException e) {
                throw new BeanReflectiveException(e);
            }
        }

        public void addAttribute(ObjectAttributeFaced schema) {
            if (attributes == null) {
                attributes = new ArrayList<>();
            }
            attributes.add(schema);
        }
    }

    @Data
    @Accessors(fluent = true)
    class ArrayResult implements ObjectFactory, ArraySchema {
        private List<? extends ObjectFactory> items;
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
    @Accessors(fluent = true, chain = true)
    class Indexable implements ObjectFactory {
        private int index;
        private Class<?> type;

        @Override
        public Object get(Arguments arguments) {
            return arguments.get(index);
        }

    }

    @Data
    @Accessors(fluent = true, chain = true)
    class IndexableBasicAttribute implements ObjectAttributeFaced {
        private int index;
        private BasicAttribute attribute;

        @Override
        public Object get(Arguments arguments) {
            Object result = arguments.get(index);
            result = attribute.databaseType().toAttributeType(result);
            return result;
        }
    }

    @Data
    @Accessors(fluent = true, chain = true)
    class IndexableProjectionAttribute implements ObjectAttributeFaced {
        private int index;
        private ProjectionBasicAttribute attribute;

        @Override
        public Object get(Arguments arguments) {
            Object result = arguments.get(index);
            result = attribute.entityAttribute().databaseType().toAttributeType(result);
            return result;
        }
    }


    interface ObjectAttributeFaced extends Attribute, ObjectFactory {

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
