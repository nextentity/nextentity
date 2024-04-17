package io.github.nextentity.jdbc;

import io.github.nextentity.core.meta.graph.EntitySchema;
import io.github.nextentity.core.meta.graph.EntityProperty;
import lombok.AllArgsConstructor;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.stream.Collectors;

public abstract class AbstractJdbcUpdateSqlBuilder implements JdbcUpdateSqlBuilder {

    @Override
    public InsertSql buildInsert(Iterable<?> entities, @NotNull EntitySchema entityType) {
        Collection<? extends EntityProperty> attributes = entityType.properties();
        EntityProperty id = entityType.id();
        HashSet<EntityProperty> nullNullAttr = new HashSet<>();
        HashSet<EntityProperty> checkAttr = new HashSet<>(attributes);
        for (Object entity : entities) {
            Iterator<? extends EntityProperty> iterator = checkAttr.iterator();
            while (iterator.hasNext()) {
                EntityProperty next = iterator.next();
                if (next.get(entity) != null) {
                    nullNullAttr.add(next);
                    iterator.remove();
                }
            }
            if (checkAttr.isEmpty()) {
                break;
            }
        }
        List<? extends EntityProperty> list = attributes.stream()
                .filter(nullNullAttr::contains)
                .collect(Collectors.toList());
        return buildInsert(entityType, list, generatedKeysBatchSupport(), nullNullAttr.contains(id));
    }

    protected boolean generatedKeysBatchSupport() {
        return true;
    }

    protected @NotNull InsertSql buildInsert(@NotNull EntitySchema entityType,
                                             Collection<? extends EntityProperty> attributes,
                                             boolean batch, boolean hasId) {
        String tableName = entityType.tableName();
        List<EntityProperty> columns = new ArrayList<>();
        StringBuilder sql = new StringBuilder("insert into ")
                .append(leftTicks())
                .append(tableName)
                .append(rightTicks())
                .append(" (");
        String delimiter = "";
        for (EntityProperty attribute : attributes) {
            if (!(attribute.isBasic())) {
                continue;
            }
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
        return new InsertSqlImpl(sql.toString(), columns, null, batch, hasId);
    }

    @NotNull
    protected abstract String rightTicks();

    @NotNull
    protected abstract String leftTicks();

    @Override
    public PreparedSql buildUpdate(@NotNull EntitySchema entityType, @NotNull List<EntityProperty> columns) {
        StringBuilder sql = new StringBuilder("update ")
                .append(leftTicks())
                .append(entityType.tableName())
                .append(rightTicks())
                .append(" set ");
        EntityProperty id = (EntityProperty) entityType.id();
        String delimiter = "";
        List<EntityProperty> cms = new ArrayList<>(columns.size() + 1);
        List<EntityProperty> versions = null;
        for (EntityProperty column : columns) {
            sql.append(delimiter).append(leftTicks()).append(column.columnName());
            if (column.isVersion()) {
                sql.append(rightTicks())
                        .append("=")
                        .append(leftTicks())
                        .append(column.columnName())
                        .append(rightTicks())
                        .append("+1");
                versions = versions == null ? new ArrayList<>(1) : versions;
                versions.add(column);
            } else {
                sql.append(rightTicks()).append("=?");
                cms.add(column);
            }
            delimiter = ",";
        }
        sql.append(" where ").append(leftTicks()).append(id.columnName()).append(rightTicks()).append("=?");
        cms.add(id);
        if (versions != null) {
            for (EntityProperty version : versions) {
                sql.append(" and ")
                        .append(leftTicks())
                        .append(version.columnName())
                        .append(rightTicks())
                        .append("=?");
                cms.add(version);
            }
        }
        return new PreparedSqlImpl(sql.toString(), cms, versions);
    }

    @Override
    public PreparedSql buildDelete(EntitySchema entity) {
        EntityProperty id = (EntityProperty) entity.id();
        String sql = "delete from " + leftTicks() + entity.tableName() + rightTicks()
                     + " where " + leftTicks() + id.columnName() + rightTicks() + "=?";
        return new PreparedSqlImpl(
                sql,
                Collections.singletonList(id),
                Collections.emptyList()
        );
    }

    @AllArgsConstructor
    private static class PreparedSqlImpl implements PreparedSql {
        private String sql;
        private List<EntityProperty> columns;
        private List<EntityProperty> versionColumns;

        @Override
        public String sql() {
            return this.sql;
        }

        @Override
        public List<EntityProperty> columns() {
            return this.columns;
        }

        @Override
        public List<EntityProperty> versionColumns() {
            return versionColumns;
        }
    }

    protected static class InsertSqlImpl extends PreparedSqlImpl implements InsertSql {
        protected boolean enableBatch;
        protected boolean hasId;


        public InsertSqlImpl(String sql, List<EntityProperty> columns, List<EntityProperty> versionColumns, boolean enableBatch, boolean hasId) {
            super(sql, columns, versionColumns);
            this.enableBatch = enableBatch;
            this.hasId = hasId;
        }

        @Override
        public boolean enableBatch() {
            return enableBatch;
        }

        @Override
        public boolean hasId() {
            return hasId;
        }

    }
}
