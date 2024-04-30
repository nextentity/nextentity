package io.github.nextentity.core.reflect.schema;

import io.github.nextentity.core.api.expression.BaseExpression;

import java.util.Iterator;
import java.util.List;

/**
 * @author HuangChengwei
 * @since 2024-04-30 15:45
 */
public interface InstanceFactory {

    Class<?> type();

    Object getInstance(Iterator<?> arguments);

    List<? extends PrimitiveFactory> primitives();

    interface ObjectFactory extends InstanceFactory {
        List<? extends AttributeFactory> attributes();
    }

    interface ArrayFactory extends InstanceFactory {
        List<? extends InstanceFactory> items();

        default Class<Object[]> type() {
            return Object[].class;
        }

        @Override
        Object[] getInstance(Iterator<?> arguments);
    }

    interface AttributeFactory extends InstanceFactory {
        Object get(Object instance);

        void set(Object instance, Object value);
    }

    interface PrimitiveFactory extends InstanceFactory, Schema {
        BaseExpression expression();

        @Override
        default Object getInstance(Iterator<?> arguments) {
            return arguments.next();
        }
    }

}
