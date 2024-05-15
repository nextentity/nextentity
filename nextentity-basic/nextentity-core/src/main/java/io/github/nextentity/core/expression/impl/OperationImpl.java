package io.github.nextentity.core.expression.impl;

import io.github.nextentity.api.Expression;
import io.github.nextentity.core.expression.Operation;
import io.github.nextentity.core.expression.Operator;
import lombok.experimental.Accessors;

import java.util.List;

@lombok.Data
@Accessors(fluent = true)
final class OperationImpl implements Operation, AbstractExpression {
    private final List<? extends Expression> operands;
    private final Operator operator;
}
