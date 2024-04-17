package io.github.nextentity.jdbc;

import io.github.nextentity.core.meta.graph.EntityProperty;
import io.github.nextentity.core.meta.graph.EntitySchema;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.Collections;

public class SqlServerUpdateSqlBuilder extends AbstractJdbcUpdateSqlBuilder {

    @Override
    protected @NotNull String leftTicks() {
        return "[";
    }

    @Override
    protected @NotNull String rightTicks() {
        return "]";
    }

    @Override
    protected boolean generatedKeysBatchSupport() {
        return false;
    }

    @Override
    protected @NotNull InsertSql buildInsert(@NotNull EntitySchema entityType,
                                             Collection<? extends EntityProperty> attributes,
                                             boolean batch,
                                             boolean hasId) {
        if (attributes.isEmpty()) {
            Collection<? extends EntityProperty> attrs = entityType.properties();
            EntityProperty pre = null;
            EntityProperty notId = null;
            for (EntityProperty attribute : attrs) {
                if (attribute != entityType.id()) {
                    notId = attribute;
                    break;
                }
                pre = attribute;
            }
            if (notId != null) {
                attributes = Collections.singletonList(notId);
            } else if (pre != null) {
                attributes = Collections.singletonList(pre);
            }
        }
        return super.buildInsert(entityType, attributes, batch, hasId);
    }
}
