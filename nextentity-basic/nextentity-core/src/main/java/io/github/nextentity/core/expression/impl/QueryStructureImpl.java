package io.github.nextentity.core.expression.impl;

import io.github.nextentity.api.Expression;
import io.github.nextentity.api.model.LockModeType;
import io.github.nextentity.api.model.Order;
import io.github.nextentity.core.expression.QueryStructure;
import io.github.nextentity.core.expression.QueryStructure.From.FromSubQuery;
import io.github.nextentity.core.expression.QueryStructure.Selected.SelectEntity;
import io.github.nextentity.core.meta.EntitySchema;
import io.github.nextentity.core.util.ImmutableList;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.List;

@Data
@Accessors(fluent = true)
class QueryStructureImpl implements FromSubQuery {

    private final Selected select;

    private final From from;

    private final Expression where;

    private final List<? extends Expression> groupBy;

    private final List<? extends Order<?>> orderBy;

    private final Expression having;

    private final Integer offset;

    private final Integer limit;

    private final LockModeType lockType;

    public QueryStructureImpl(QueryStructure queryStructure) {
        this(
                queryStructure.select(),
                queryStructure.from(),
                queryStructure.where(),
                queryStructure.groupBy(),
                queryStructure.orderBy(),
                queryStructure.having(),
                queryStructure.offset(),
                queryStructure.limit(),
                queryStructure.lockType()
        );
    }

    public QueryStructureImpl(Selected select, From from) {
        this(select,
                from,
                ExpressionImpls.TRUE,
                ImmutableList.of(),
                ImmutableList.of(),
                ExpressionImpls.TRUE,
                null,
                null,
                LockModeType.NONE);
    }

    public QueryStructureImpl(Selected select,
                              From from,
                              Expression where,
                              List<? extends Expression> groupBy,
                              List<? extends Order<?>> orderBy,
                              Expression having,
                              Integer offset,
                              Integer limit,
                              LockModeType lockType) {
        this.select = select;
        this.from = from;
        this.where = where;
        this.groupBy = groupBy;
        this.orderBy = orderBy;
        this.having = having;
        this.offset = offset;
        this.limit = limit;
        this.lockType = lockType;
    }


    public QueryStructureImpl(Class<?> entityType) {
        this(new SelectEntity().type(entityType), new FromEntityImpl(entityType));
    }

    public QueryStructureImpl(EntitySchema entityType) {
        this(entityType.type());
    }

}
