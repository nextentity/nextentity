package io.github.nextentity.core.expression.impl;

import io.github.nextentity.core.expression.Literal;
import lombok.experimental.Accessors;

@lombok.Data
@Accessors(fluent = true)
final class LiteralImpl implements Literal, AbstractExpression {
    static final Literal TRUE = new LiteralImpl(true);
    static final Literal FALSE = new LiteralImpl(false);

    private final Object value;
}
