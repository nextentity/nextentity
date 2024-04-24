package io.github.nextentity.jdbc;

import io.github.nextentity.core.meta.BasicAttribute;
import io.github.nextentity.core.meta.EntityType;
import io.github.nextentity.core.util.ImmutableList;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

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
        BasicAttribute idAttribute = entityType.id();
        Collection<? extends BasicAttribute> basicAttributes = entityType.primitiveAttributes();
        List<? extends BasicAttribute> withoutId = basicAttributes.stream()
                .filter(attr -> attr != idAttribute)
                .collect(ImmutableList.collector(basicAttributes.size() - 1));
        List<InsertSqlStatement> result = new ArrayList<>();
        List<Object> entitiesHasId = new ArrayList<>();
        for (Object entity : entities) {
            List<?> single = Collections.singletonList(entity);
            if (idAttribute.get(entity) == null) {
                result.add(buildInsertStatement(single, entityType, withoutId, true));
            } else {
                entitiesHasId.add(entity);
            }
        }
        if (!entitiesHasId.isEmpty()) {
            result.add(buildInsertStatement(entitiesHasId, entityType, basicAttributes, false));
        }
        return result;
    }
}
