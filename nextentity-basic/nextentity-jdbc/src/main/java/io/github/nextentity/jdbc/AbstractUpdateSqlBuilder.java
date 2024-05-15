package io.github.nextentity.jdbc;

import io.github.nextentity.core.meta.BasicAttribute;
import io.github.nextentity.core.meta.EntitySchema;
import io.github.nextentity.core.meta.EntityType;
import io.github.nextentity.core.util.Iterators;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

public abstract class AbstractUpdateSqlBuilder implements JdbcUpdateSqlBuilder {

    @Override
    public List<InsertSqlStatement> buildInsertStatement(Iterable<?> entities, @NotNull EntityType entityType) {
        BasicAttribute idAttribute = entityType.id();
        boolean hasNullId = false;
        for (Object entity : entities) {
            Object id = idAttribute.getDatabaseValue(entity);
            if (id == null) {
                hasNullId = true;
            }
        }
        Collection<? extends BasicAttribute> selectList = entityType.primitiveAttributes();
        return Collections.singletonList(buildInsertStatement(entities, entityType, selectList, hasNullId));
    }

    protected InsertSqlStatement buildInsertStatement(Iterable<?> entities,
                                                      EntityType entityType,
                                                      Collection<? extends BasicAttribute> attributes,
                                                      boolean generateKey) {
        String tableName = entityType.tableName();
        List<BasicAttribute> columns = new ArrayList<>();
        StringBuilder sql = new StringBuilder("insert into ")
                .append(leftTicks())
                .append(tableName)
                .append(rightTicks())
                .append(" (");
        String delimiter = "";
        for (BasicAttribute attribute : attributes) {
            sql.append(delimiter).append(leftTicks()).append(attribute.columnName()).append(rightTicks());
            columns.add(attribute);
            delimiter = ",";
        }
        sql.append(") values (");
        delimiter = "";
        int size = columns.size();
        for (int i = 0; i < size; i++) {
            sql.append(delimiter).append("?");
            delimiter = ",";
        }
        sql.append(")");
        Iterable<? extends Iterable<?>> parameters = getParameters(entities, attributes);
        return new InsertSqlStatement(entities, sql.toString(), parameters, generateKey);
    }

    private static Iterable<? extends Iterable<?>> getParameters(Iterable<?> entities,
                                                                 Collection<? extends BasicAttribute> attributes) {
        return Iterators.map(entities, entity -> Iterators.map(attributes, attr -> attr.getDatabaseValue(entity)));
    }

    @NotNull
    protected abstract String rightTicks();

    @NotNull
    protected abstract String leftTicks();

    @Override
    public BatchSqlStatement buildUpdateStatement(Iterable<?> entities,
                                                  EntitySchema entityType,
                                                  boolean excludeNull) {
        Collection<? extends BasicAttribute> columns = entityType.primitiveAttributes();
        StringBuilder sql = new StringBuilder("update ")
                .append(leftTicks())
                .append(entityType.tableName())
                .append(rightTicks())
                .append(" set ");
        BasicAttribute id = entityType.id();
        String delimiter = "";
        List<BasicAttribute> paramAttr = new ArrayList<>(columns.size() + 1);
        BasicAttribute version = entityType.version();
        for (BasicAttribute attribute : columns) {
            if (entityType.id() == attribute) {
                continue;
            }
            sql.append(delimiter);
            delimiter = ",";
            sql.append(leftTicks()).append(attribute.columnName()).append(rightTicks()).append("=");
            if (excludeNull) {
                sql.append("case when ? is null then")
                        .append(leftTicks()).append(attribute.columnName()).append(rightTicks())
                        .append("else ? end");
                paramAttr.add(attribute);
                paramAttr.add(attribute);
            } else {
                if (attribute == version) {
                    sql.append("?+1");
                } else {
                    sql.append("?");
                }
                paramAttr.add(attribute);
            }
        }
        sql.append(" where ").append(leftTicks()).append(id.columnName()).append(rightTicks()).append("=?");
        paramAttr.add(id);
        if (version != null) {
            sql.append(" and ")
                    .append(leftTicks())
                    .append(version.columnName())
                    .append(rightTicks())
                    .append("=?");
            paramAttr.add(version);
        }
        return new BatchSqlStatement(sql.toString(), getParameters(entities, paramAttr));
    }

    @Override
    public BatchSqlStatement buildDeleteStatement(Iterable<?> entities, EntityType entity) {
        BasicAttribute id = entity.id();
        String sql = "delete from " + leftTicks() + entity.tableName() + rightTicks()
                     + " where " + leftTicks() + id.columnName() + rightTicks() + "=?";
        List<BasicAttribute> paramAttr = Collections.singletonList(id);
        Iterable<? extends Iterable<?>> parameters = getParameters(entities, paramAttr);
        return new BatchSqlStatement(sql, parameters);
    }

}
