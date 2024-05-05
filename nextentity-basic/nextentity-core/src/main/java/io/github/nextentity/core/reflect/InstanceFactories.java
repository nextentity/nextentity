package io.github.nextentity.core.reflect;

import io.github.nextentity.api.Expression;
import io.github.nextentity.api.model.Tuple;
import io.github.nextentity.core.Tuples;
import io.github.nextentity.core.exception.UncheckedReflectiveException;
import io.github.nextentity.core.expression.EntityPath;
import io.github.nextentity.core.meta.BasicAttribute;
import io.github.nextentity.core.meta.EntitySchema;
import io.github.nextentity.core.meta.EntityType;
import io.github.nextentity.core.meta.ProjectionAssociationAttribute;
import io.github.nextentity.core.meta.ProjectionBasicAttribute;
import io.github.nextentity.core.reflect.schema.Attribute;
import io.github.nextentity.core.reflect.schema.InstanceFactory;
import io.github.nextentity.core.reflect.schema.InstanceFactory.AttributeFactory;
import io.github.nextentity.core.reflect.schema.InstanceFactory.PrimitiveFactory;
import io.github.nextentity.core.util.ImmutableList;
import lombok.extern.slf4j.Slf4j;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.lang.reflect.RecordComponent;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * @author HuangChengwei
 * @since 2024-04-30 16:12
 */
@Slf4j
public class InstanceFactories {

    public static InstanceFactory.ObjectFactory fetch(EntityType entityType, Iterable<? extends EntityPath> fetchPaths) {
        ObjectFactoryBuilder builder = new ObjectFactoryBuilder(entityType);
        for (EntityPath fetchPath : fetchPaths) {
            builder.fetch(fetchPath);
        }
        return builder.build();
    }

    private static class AttributeFactoryBuilder extends AbstractInstanceFactoryBuilder<AttributeFactory> {

        private AttributeFactoryBuilder(EntitySchema schema) {
            super(schema);
        }

        @Override
        protected AttributeFactory build(ImmutableList<AttributeFactory> attrs) {
            return new ObjectAttributeFactory(attrs, (BasicAttribute) schema);
        }

    }

    private static class ObjectFactoryBuilder extends AbstractInstanceFactoryBuilder<InstanceFactory.ObjectFactory> {

        private ObjectFactoryBuilder(EntitySchema schema) {
            super(schema);
        }

        @Override
        protected InstanceFactory.ObjectFactory build(ImmutableList<AttributeFactory> attrs) {
            return new ObjectFactoryImpl(attrs, schema.type());
        }
    }

    private static abstract class AbstractInstanceFactoryBuilder<T> {
        protected final EntitySchema schema;
        Map<String, AttributeFactoryBuilder> attributes = new LinkedHashMap<>();

        private AbstractInstanceFactoryBuilder(EntitySchema schema) {
            this.schema = schema;
        }

        public void fetch(EntityPath path) {
            AbstractInstanceFactoryBuilder<?> cur = this;
            for (String s : path) {
                cur = cur.fetch(s);
                if (cur == null) {
                    log.warn("attempted to fetch a none-entity attribute `{}` of {}",
                            path.stream().collect(Collectors.joining(".")),
                            schema.type().getName());
                    return;
                }
            }
        }

        public AbstractInstanceFactoryBuilder<?> fetch(String attribute) {
            BasicAttribute attr = schema.getAttribute(attribute);
            if (attr.isPrimitive()) {
                return null;
            }
            return attributes.computeIfAbsent(attribute, k -> new AttributeFactoryBuilder((EntitySchema) attr));
        }

        public T build() {
            InstanceFactory.ObjectFactory factory = schema.getInstanceFactory();
            Stream<? extends AttributeFactory> stream = factory.attributes().stream();
            ImmutableList<InstanceFactory.AttributeFactory> attrs = Stream
                    .concat(stream, attributes.values().stream().map(AttributeFactoryBuilder::build))
                    .collect(ImmutableList.collector(factory.attributes().size() + attributes.size()));
            return build(attrs);
        }

        protected abstract T build(ImmutableList<AttributeFactory> attrs);
    }

    private static Constructor<?> getConstructor(Class<?> type) {
        try {
            return type.getConstructor();
        } catch (NoSuchMethodException e) {
            throw new UncheckedReflectiveException("no parameterless constructor found for " + type.getName(), e);
        }
    }

    private static ImmutableList<? extends PrimitiveFactory> toPrimitives(Collection<? extends InstanceFactory> attributes) {
        final ImmutableList<? extends PrimitiveFactory> primitives;
        int size = attributes.stream()
                .map(InstanceFactory::primitives)
                .mapToInt(Collection::size)
                .sum();
        primitives = attributes.stream()
                .map(InstanceFactory::primitives)
                .flatMap(Collection::stream)
                .collect(ImmutableList.collector(size));
        return primitives;
    }

    static abstract class AbstractObjectFactory implements InstanceFactory.ObjectFactory {
        private Function<Iterator<?>, Object> factory;
        private final Class<?> type;

        public AbstractObjectFactory(Class<?> type) {
            this.type = type;
            if (type.isRecord()) {
                factory = new RecordFactoryInitializer();
            } else if (type.isInterface()) {
                factory = new InterfaceFactory();
            } else {
                factory = new ObjectFactory();
            }
        }

        public Object getInstance(Iterator<?> arguments) {
            return factory.apply(arguments);
        }

        public Class<?> type() {
            return type;
        }

        class ObjectFactory implements Function<Iterator<?>, Object> {
            private final Constructor<?> constructor;

            ObjectFactory() {
                this.constructor = getConstructor(type());
            }

            public Object apply(Iterator<?> arguments) {
                Object object = null;
                for (AttributeFactory attribute : attributes()) {
                    Object value = attribute.getInstance(arguments);
                    if (value != null) {
                        if (object == null) {
                            try {
                                object = constructor.newInstance();
                            } catch (ReflectiveOperationException e) {
                                throw new UncheckedReflectiveException("new instance error", e);
                            }
                        }
                        attribute.set(object, value);
                    }
                }
                return object;
            }
        }

        class InterfaceFactory implements Function<Iterator<?>, Object> {
            public Object apply(Iterator<?> arguments) {
                Map<Method, Object> map = new HashMap<>();
                boolean notNull = false;
                for (AttributeFactory attribute : attributes()) {
                    Object value = attribute.getInstance(arguments);
                    notNull = notNull || value != null;
                    map.put(attribute.getter(), value);
                }
                if (notNull) {
                    return ReflectUtil.newProxyInstance(type(), map);
                } else {
                    return null;
                }
            }
        }

        class RecordFactoryInitializer implements Function<Iterator<?>, Object> {
            @Override
            public synchronized Object apply(Iterator<?> iterator) {
                if (factory == this) {
                    factory = new RecordFactory();
                }
                return factory.apply(iterator);
            }
        }

        class RecordFactory implements Function<Iterator<?>, Object> {
            private final int[] attributePositions;
            private final Constructor<?> constructor;
            private final int constructorArgumentsLength;

            public RecordFactory() {
                RecordComponent[] components = type().getRecordComponents();
                constructorArgumentsLength = components.length;
                Class<?>[] parameterTypes = new Class[constructorArgumentsLength];
                Map<String, Integer> positionMap = new HashMap<>();
                for (int i = 0; i < components.length; i++) {
                    RecordComponent component = components[i];
                    parameterTypes[i] = component.getType();
                    positionMap.put(component.getName(), i);
                }
                try {
                    constructor = type().getDeclaredConstructor(parameterTypes);
                } catch (NoSuchMethodException e) {
                    throw new UncheckedReflectiveException("no such constructor", e);
                }
                int cur = 0;
                attributePositions = new int[attributes().size()];
                Arrays.fill(attributePositions, -1);
                for (AttributeFactory attribute : attributes()) {
                    Integer i = positionMap.get(attribute.name());
                    if (i != null) {
                        attributePositions[cur++] = i;
                    }
                }
            }

            public Object apply(Iterator<?> arguments) {
                Object[] args = null;
                List<? extends AttributeFactory> attributes = attributes();
                for (int i = 0; i < attributes.size(); i++) {
                    AttributeFactory attribute = attributes.get(i);
                    Object arg = attribute.getInstance(arguments);
                    if (arg != null) {
                        int position = attributePositions[i];
                        if (position >= 0) {
                            if (args == null) {
                                args = new Object[constructorArgumentsLength];
                            }
                            args[position] = arg;
                        }
                    }
                }
                if (args == null) {
                    return null;
                }
                try {
                    return constructor.newInstance(args);
                } catch (ReflectiveOperationException e) {
                    throw new UncheckedReflectiveException("new instance error", e);
                }
            }
        }


    }

    interface AbstractAttribute extends AttributeFactory {
        Attribute attribute();

        @Override
        default Object get(Object instance) {
            return attribute().get(instance);
        }

        @Override
        default void set(Object instance, Object value) {
            attribute().set(instance, value);
        }

        @Override
        default Method getter() {
            return attribute().getter();
        }

        @Override
        default String name() {
            return attribute().name();
        }

    }


    public static class ObjectFactoryImpl extends AbstractObjectFactory {
        private final ImmutableList<? extends AttributeFactory> attributes;
        private final ImmutableList<? extends PrimitiveFactory> primitives;

        public ObjectFactoryImpl(ImmutableList<? extends AttributeFactory> attributes, Class<?> type) {
            super(type);
            this.attributes = attributes;
            this.primitives = toPrimitives(attributes);
        }

        @Override
        public List<? extends AttributeFactory> attributes() {
            return attributes;
        }

        @Override
        public List<? extends PrimitiveFactory> primitives() {
            return primitives;
        }
    }

    public static class ArrayFactoryImpl implements InstanceFactory.ArrayFactory {
        private final ImmutableList<? extends InstanceFactory> items;
        private final ImmutableList<? extends PrimitiveFactory> primitives;

        public ArrayFactoryImpl(ImmutableList<? extends InstanceFactory> items) {
            this.items = items;
            this.primitives = toPrimitives(items);
        }

        @Override
        public List<? extends InstanceFactory> items() {
            return items;
        }

        @Override
        public Tuple getInstance(Iterator<?> arguments) {
            Object[] result = new Object[items().size()];
            for (int i = 0; i < items().size(); i++) {
                InstanceFactory factory = items.get(i);
                result[i] = factory.getInstance(arguments);
            }
            return Tuples.of(result);
        }

        @Override
        public List<? extends PrimitiveFactory> primitives() {
            return primitives;
        }
    }

    public static class PrimitiveFactoryImpl implements PrimitiveFactory {
        private final ImmutableList<? extends PrimitiveFactory> primitives = ImmutableList.of(this);
        private final Expression expression;
        private final Class<?> type;

        public PrimitiveFactoryImpl(Expression expression, Class<?> type) {
            this.expression = expression;
            this.type = type;
        }

        @Override
        public Expression expression() {
            return expression;
        }

        @Override
        public Class<?> type() {
            return type;
        }


        @Override
        public List<? extends PrimitiveFactory> primitives() {
            return primitives;
        }
    }

    public static class AttributeFactoryImpl implements AbstractAttribute, PrimitiveFactory {
        private final ImmutableList<? extends PrimitiveFactory> primitives = ImmutableList.of(this);
        private final BasicAttribute attribute;

        public AttributeFactoryImpl(BasicAttribute attribute) {
            this.attribute = attribute;
        }

        @Override
        public Attribute attribute() {
            return attribute;
        }

        @Override
        public EntityPath expression() {
            return attribute.path();
        }

        @Override
        public Class<?> type() {
            return attribute().type();
        }

        @Override
        public Object getInstance(Iterator<?> arguments) {
            Object instance = PrimitiveFactory.super.getInstance(arguments);
            return attribute.databaseType().toAttributeType(instance);
        }

        @Override
        public List<? extends PrimitiveFactory> primitives() {
            return primitives;
        }
    }


    public static class ObjectAttributeFactory extends AbstractObjectFactory implements AbstractAttribute {
        private final ImmutableList<? extends AttributeFactory> attributes;
        private final BasicAttribute attribute;
        private final ImmutableList<? extends PrimitiveFactory> primitives;

        public ObjectAttributeFactory(ImmutableList<? extends AttributeFactory> attributes, BasicAttribute attribute) {
            super(attribute.type());
            this.attributes = attributes;
            this.attribute = attribute;
            this.primitives = toPrimitives(attributes);
        }

        @Override
        public List<? extends AttributeFactory> attributes() {
            return attributes;
        }

        @Override
        public List<? extends PrimitiveFactory> primitives() {
            return primitives;
        }

        @Override
        public Attribute attribute() {
            return attribute;
        }
    }

    public static class ProjectionAttributeFactory implements AbstractAttribute, PrimitiveFactory {
        private final ImmutableList<? extends PrimitiveFactory> primitives = ImmutableList.of(this);
        private final ProjectionBasicAttribute attribute;

        public ProjectionAttributeFactory(ProjectionBasicAttribute attribute) {
            this.attribute = attribute;
        }

        @Override
        public Expression expression() {
            return attribute.entityAttribute().path();
        }

        @Override
        public Class<?> type() {
            return attribute().type();
        }

        @Override
        public List<? extends PrimitiveFactory> primitives() {
            return primitives;
        }

        @Override
        public Attribute attribute() {
            return attribute;
        }
    }

    public static class ProjectionObjectAttributeFactory extends AbstractObjectFactory implements AbstractAttribute {
        private final ImmutableList<? extends AttributeFactory> attributes;
        private final ProjectionAssociationAttribute attribute;
        private final ImmutableList<? extends PrimitiveFactory> primitives;

        public ProjectionObjectAttributeFactory(ImmutableList<? extends AttributeFactory> attributes,
                                                ProjectionAssociationAttribute attribute) {
            super(attribute.type());
            this.attributes = attributes;
            this.attribute = attribute;
            this.primitives = toPrimitives(attributes);
        }

        @Override
        public List<? extends AttributeFactory> attributes() {
            return attributes;
        }

        @Override
        public List<? extends PrimitiveFactory> primitives() {
            return primitives;
        }

        @Override
        public Attribute attribute() {
            return attribute;
        }
    }

}
