package io.github.nextentity.jdbc;

import io.github.nextentity.core.meta.graph.EntityProperty;
import io.github.nextentity.core.meta.graph.EntitySchema;
import org.jetbrains.annotations.NotNull;

import java.util.List;
import java.util.stream.Collectors;

public interface JdbcUpdateSqlBuilder {

    InsertSql buildInsert(Iterable<?> entities, @NotNull EntitySchema entityType);

    default PreparedSql buildUpdate(@NotNull EntitySchema entityType) {
        EntityProperty id = entityType.id();
        List<EntityProperty> basicAttributes = entityType.properties().stream()
                .filter(it -> it.isBasic() && it != id)
                .map(it -> (EntityProperty) it)
                .collect(Collectors.toList());
        return buildUpdate(entityType, basicAttributes);
    }

    PreparedSql buildUpdate(@NotNull EntitySchema entityType, @NotNull List<EntityProperty> columns);

    PreparedSql buildDelete(EntitySchema entity);

    interface PreparedSql {
        String sql();

        List<EntityProperty> columns();

        List<EntityProperty> versionColumns();

    }

    interface InsertSql extends PreparedSql {
        boolean enableBatch();

        boolean hasId();
    }

}
