package io.github.nextentity.core.reflect.schema;

import io.github.nextentity.api.Expression;
import io.github.nextentity.api.model.Tuple;

import java.lang.reflect.Method;
import java.util.Iterator;
import java.util.List;

/**
 * @author HuangChengwei
 * @since 2024-04-30 15:45
 */
public interface InstanceFactory {

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
        Tuple getInstance(Iterator<?> arguments);
    }

    interface AttributeFactory extends InstanceFactory {
        Object get(Object instance);

        void set(Object instance, Object value);

        Method getter();

        String name();
    }

    interface PrimitiveFactory extends InstanceFactory, Typed {
        Expression expression();

        @Override
        default Object getInstance(Iterator<?> arguments) {
            return arguments.next();
        }
    }

}
