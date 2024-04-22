package io.github.nextentity.jdbc;

import io.github.nextentity.core.meta.BasicAttribute;
import io.github.nextentity.core.meta.EntityType;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

public class SqlServerUpdateSqlBuilder extends AbstractUpdateSqlBuilder {

    @Override
    protected @NotNull String leftTicks() {
        return "[";
    }

    @Override
    protected @NotNull String rightTicks() {
        return "]";
    }

    @Override
    public List<InsertSqlStatement> buildInsertStatement(Iterable<?> entities, @NotNull EntityType entityType) {
        Collection<? extends BasicAttribute> basicAttributes = entityType.primitiveAttributes();
        List<? extends BasicAttribute> withoutId = basicAttributes.stream()
                .filter(attr -> attr != entityType.id())
                .collect(Collectors.toList());
        return StreamSupport.stream(entities.spliterator(), false)
                .map(entity -> {
                    BasicAttribute id = entityType.id();
                    List<?> single = Collections.singletonList(entity);
                    if (id.get(entity) == null) {
                        return buildInsertStatement(single, entityType, withoutId, true);
                    } else {
                        return buildInsertStatement(single, entityType, basicAttributes, false);
                    }
                })
                .collect(Collectors.toList());
    }
}
