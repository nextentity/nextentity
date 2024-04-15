package io.github.nextentity.jdbc;

import io.github.nextentity.core.meta.Attribute;
import io.github.nextentity.core.meta.EntityType;
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
    protected @NotNull InsertSql buildInsert(@NotNull EntityType entityType,
                                             Collection<? extends Attribute> attributes,
                                             boolean batch,
                                             boolean hasId) {
        if (attributes.isEmpty()) {
            Collection<? extends Attribute> attrs = entityType.attributes();
            Attribute pre = null;
            Attribute notId = null;
            for (Attribute attribute : attrs) {
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
