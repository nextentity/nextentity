package io.github.nextentity.jdbc;

import io.github.nextentity.core.meta.Attribute;
import io.github.nextentity.core.meta.BasicAttribute;
import io.github.nextentity.core.meta.EntityType;
import org.jetbrains.annotations.NotNull;

import java.util.List;
import java.util.stream.Collectors;

public interface JdbcUpdateSqlBuilder {

    InsertSql buildInsert(Iterable<?> entities, @NotNull EntityType entityType);

    default PreparedSql buildUpdate(@NotNull EntityType entityType) {
        Attribute id = entityType.id();
        List<BasicAttribute> basicAttributes = entityType.attributes().stream()
                .filter(it -> it instanceof BasicAttribute && it != id)
                .map(it -> (BasicAttribute) it)
                .collect(Collectors.toList());
        return buildUpdate(entityType, basicAttributes);
    }

    PreparedSql buildUpdate(@NotNull EntityType entityType, @NotNull List<BasicAttribute> columns);

    PreparedSql buildDelete(EntityType entity);

    interface PreparedSql {
        String sql();

        List<BasicAttribute> columns();

        List<BasicAttribute> versionColumns();

    }

    interface InsertSql extends PreparedSql {
        boolean enableBatch();

        boolean hasId();
    }

}
