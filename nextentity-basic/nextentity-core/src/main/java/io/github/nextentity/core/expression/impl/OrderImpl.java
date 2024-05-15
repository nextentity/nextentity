package io.github.nextentity.core.expression.impl;

import io.github.nextentity.api.Expression;
import io.github.nextentity.api.model.Order;
import io.github.nextentity.api.SortOrder;
import lombok.experimental.Accessors;

@lombok.Data
@Accessors(fluent = true)
final class OrderImpl<T> implements Order<T> {
    private final Expression expression;
    private final SortOrder order;
}
