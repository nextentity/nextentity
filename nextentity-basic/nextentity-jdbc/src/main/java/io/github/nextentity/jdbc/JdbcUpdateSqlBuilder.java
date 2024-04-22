package io.github.nextentity.jdbc;

import io.github.nextentity.core.meta.EntitySchema;
import io.github.nextentity.core.meta.EntityType;

import java.util.List;

public interface JdbcUpdateSqlBuilder {

    List<InsertSqlStatement> buildInsertStatement(Iterable<?> entities, EntityType entityType);

    BatchSqlStatement buildUpdateStatement(Iterable<?> entities,
                                           EntitySchema entityType,
                                           boolean excludeNull);

    BatchSqlStatement buildDeleteStatement(Iterable<?> entities, EntityType entity);

}
