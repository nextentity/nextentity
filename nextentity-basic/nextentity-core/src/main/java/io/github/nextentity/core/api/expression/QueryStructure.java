package io.github.nextentity.core.api.expression;

import io.github.nextentity.core.api.LockModeType;
import io.github.nextentity.core.api.Order;
import io.github.nextentity.core.reflect.schema.ArraySchema;
import io.github.nextentity.core.reflect.schema.Schema;
import io.github.nextentity.core.util.tuple.Tuple;
import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;

/**
 * @author HuangChengwei
 * @since 2024/4/17 下午1:28
 */
public interface QueryStructure extends BaseExpression {

    Selected select();

    From from();

    BaseExpression where();

    List<? extends BaseExpression> groupBy();

    List<? extends Order<?>> orderBy();

    BaseExpression having();

    Integer offset();

    Integer limit();

    LockModeType lockType();

    interface From extends Serializable {

        Class<?> type();

        interface Entity extends From {
        }

        interface FromSubQuery extends From, QueryStructure {
            @Override
            default Class<?> type() {
                return select().type();
            }

        }

    }

    interface Selected extends Schema {
        boolean distinct();

        @Data
        @Accessors(fluent = true, chain = true)
        class SelectPrimitive implements Selected {
            private Class<?> type = Object.class;
            private boolean distinct;
            private BaseExpression expression;

            public SelectPrimitive() {
            }
        }

        @Data
        @Accessors(fluent = true, chain = true)
        class SelectArray implements Selected, ArraySchema {
            private boolean distinct;
            private Collection<? extends Selected> items;

            @Override
            public Class<?> type() {
                return Tuple.class;
            }
        }

        @Data
        @Accessors(fluent = true, chain = true)
        class SelectEntity implements Selected {
            private Class<?> type;
            private boolean distinct;
            private Collection<? extends EntityPath> fetch;

            public SelectEntity() {
            }

            public SelectEntity(SelectEntity select) {
                this.type = select.type();
                this.distinct = select.distinct();
                this.fetch = select.fetch();
            }
        }

        @Data
        @Accessors(fluent = true, chain = true)
        class SelectProjection implements Selected {
            private Class<?> type;
            private Class<?> entityType;
            private boolean distinct;
        }


    }
}
