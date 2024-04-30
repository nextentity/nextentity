package io.github.nextentity.core.reflect;

import io.github.nextentity.core.api.expression.BaseExpression;
import io.github.nextentity.core.api.expression.EntityPath;
import io.github.nextentity.core.exception.UncheckedReflectiveException;
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

import java.lang.reflect.Constructor;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Stream;

/**
 * @author HuangChengwei
 * @since 2024-04-30 16:12
 */
public class InstanceFactories {

    public static InstanceFactory.ObjectFactory fetch(EntityType entityType, List<EntityPath> fetchPaths) {
        ObjectBuilder builder = new ObjectBuilder(entityType);
        for (EntityPath fetchPath : fetchPaths) {
            builder.fetch(fetchPath);
        }
        return builder.build();
    }

    private static class AttributeBuilder extends AbstractObjectFactoryBuilder<AttributeFactory> {

        private AttributeBuilder(EntitySchema schema) {
            super(schema);
        }

        @Override
        protected AttributeFactory build(ImmutableList<AttributeFactory> attrs) {
            return new ObjectAttribute(attrs, (BasicAttribute) schema);
        }

    }

    private static class ObjectBuilder extends AbstractObjectFactoryBuilder<InstanceFactory.ObjectFactory> {

        private ObjectBuilder(EntitySchema schema) {
            super(schema);
        }

        @Override
        protected InstanceFactory.ObjectFactory build(ImmutableList<AttributeFactory> attrs) {
            return new ObjectFactoryImpl(attrs, schema.type());
        }
    }

    private static abstract class AbstractObjectFactoryBuilder<T> {
        protected final EntitySchema schema;
        Map<String, AttributeBuilder> attributes = new LinkedHashMap<>();

        private AbstractObjectFactoryBuilder(EntitySchema schema) {
            this.schema = schema;
        }

        public void fetch(EntityPath path) {
            AbstractObjectFactoryBuilder<?> cur = this;
            for (String s : path) {
                cur = cur.fetch(s);
            }
        }

        public AbstractObjectFactoryBuilder<?> fetch(String attribute) {
            BasicAttribute attr = schema.getAttribute(attribute);
            if (attr.isPrimitive()) {
                throw new IllegalArgumentException();
            }
            return attributes.computeIfAbsent(attribute, k -> new AttributeBuilder((EntitySchema) attr));
        }

        public T build() {
            InstanceFactory.ObjectFactory factory = schema.getInstanceFactory();
            Stream<? extends AttributeFactory> stream = factory.attributes().stream();
            ImmutableList<InstanceFactory.AttributeFactory> attrs = Stream
                    .concat(stream, attributes.values().stream().map(AttributeBuilder::build))
                    .collect(ImmutableList.collector(factory.attributes().size() + attributes.size()));
            return build(attrs);
        }

        protected abstract T build(ImmutableList<AttributeFactory> attrs);
    }

    private static Constructor<?> getConstructor(Class<?> type) {
        try {
            return type.getConstructor();
        } catch (NoSuchMethodException e) {
            throw new UncheckedReflectiveException("unable to get parameterless constructor", e);
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
        public Object getInstance(Iterator<?> arguments) {
            Object object = null;
            for (AttributeFactory attribute : attributes()) {
                Object value = attribute.getInstance(arguments);
                if (value != null) {
                    if (object == null) {
                        try {
                            object = constructor().newInstance();
                        } catch (ReflectiveOperationException e) {
                            throw new UncheckedReflectiveException("new instance error", e);
                        }
                    }
                    attribute.set(object, value);
                }
            }
            return object;
        }

        protected abstract Constructor<?> constructor();

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
        default Class<?> type() {
            return attribute().type();
        }

    }

    public static class ObjectFactoryImpl extends AbstractObjectFactory {
        private final ImmutableList<? extends AttributeFactory> attributes;
        private final Class<?> type;
        private final Constructor<?> constructor;
        private final ImmutableList<? extends PrimitiveFactory> primitives;

        public ObjectFactoryImpl(ImmutableList<? extends AttributeFactory> attributes, Class<?> type) {
            this.attributes = attributes;
            this.type = type;
            this.constructor = getConstructor(type);
            this.primitives = toPrimitives(attributes);
        }

        @Override
        protected Constructor<?> constructor() {
            return constructor;
        }

        @Override
        public List<? extends AttributeFactory> attributes() {
            return attributes;
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
        public Object[] getInstance(Iterator<?> arguments) {
            Object[] result = new Object[items().size()];
            for (int i = 0; i < items().size(); i++) {
                InstanceFactory factory = items.get(i);
                result[i] = factory.getInstance(arguments);
            }
            return result;
        }

        @Override
        public List<? extends PrimitiveFactory> primitives() {
            return primitives;
        }
    }

    public static class PrimitiveAttribute implements AbstractAttribute, PrimitiveFactory {
        private final ImmutableList<? extends PrimitiveFactory> primitives = ImmutableList.of(this);
        private final BasicAttribute attribute;

        public PrimitiveAttribute(BasicAttribute attribute) {
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
        public Object getInstance(Iterator<?> arguments) {
            Object result = arguments.next();
            result = attribute.databaseType().toAttributeType(result);
            return result;
        }

        @Override
        public List<? extends PrimitiveFactory> primitives() {
            return primitives;
        }
    }

    public static class ObjectAttribute extends AbstractObjectFactory implements AbstractAttribute {
        private final ImmutableList<? extends AttributeFactory> attributes;
        private final BasicAttribute attribute;
        private final Constructor<?> constructor;
        private final ImmutableList<? extends PrimitiveFactory> primitives;

        public ObjectAttribute(ImmutableList<? extends AttributeFactory> attributes, BasicAttribute attribute) {
            this.attributes = attributes;
            this.attribute = attribute;
            this.constructor = getConstructor(attribute.type());
            this.primitives = toPrimitives(attributes);
        }

        @Override
        protected Constructor<?> constructor() {
            return constructor;
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

    public static class ProjectionPrimitiveAttribute implements AbstractAttribute, PrimitiveFactory {
        private final ImmutableList<? extends PrimitiveFactory> primitives = ImmutableList.of(this);
        private final ProjectionBasicAttribute attribute;

        public ProjectionPrimitiveAttribute(ProjectionBasicAttribute attribute) {
            this.attribute = attribute;
        }

        @Override
        public BaseExpression expression() {
            return attribute.entityAttribute().path();
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

    public static class ProjectionObjectAttribute extends AbstractObjectFactory implements AbstractAttribute {
        private final ImmutableList<? extends AttributeFactory> attributes;
        private final ProjectionAssociationAttribute attribute;
        private final Constructor<?> constructor;
        private final ImmutableList<? extends PrimitiveFactory> primitives;

        public ProjectionObjectAttribute(ImmutableList<? extends AttributeFactory> attributes,
                                         ProjectionAssociationAttribute attribute) {
            this.attributes = attributes;
            this.attribute = attribute;
            this.constructor = getConstructor(attribute.type());
            this.primitives = toPrimitives(attributes);
        }

        @Override
        protected Constructor<?> constructor() {
            return constructor;
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

    public static class PrimitiveFactoryImpl implements PrimitiveFactory {
        private final ImmutableList<? extends PrimitiveFactory> primitives = ImmutableList.of(this);
        private final BaseExpression expression;
        private final Class<?> type;

        public PrimitiveFactoryImpl(BaseExpression expression, Class<?> type) {
            this.expression = expression;
            this.type = type;
        }

        @Override
        public BaseExpression expression() {
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

}
