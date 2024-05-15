package io.github.nextentity.core.expression.impl;

import io.github.nextentity.core.expression.QueryStructure.From;
import lombok.experimental.Accessors;

import static io.github.nextentity.core.expression.QueryStructure.From.*;

@lombok.Data
@Accessors(fluent = true)
final class FromEntityImpl implements FromEntity {
    private final Class<?> type;
}
