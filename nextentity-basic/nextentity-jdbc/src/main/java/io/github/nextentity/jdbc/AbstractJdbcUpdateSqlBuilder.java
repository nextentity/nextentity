package io.github.nextentity.jdbc;

import io.github.nextentity.core.meta.Attribute;
import io.github.nextentity.core.meta.BasicAttribute;
import io.github.nextentity.core.meta.EntityType;
import lombok.AllArgsConstructor;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

@SuppressWarnings("PatternVariableCanBeUsed")
public abstract class AbstractJdbcUpdateSqlBuilder implements JdbcUpdateSqlBuilder {

    @Override
    public PreparedSql buildInsert(@NotNull EntityType entityType) {
        String tableName = entityType.tableName();
        List<BasicAttribute> columns = new ArrayList<>();
        StringBuilder sql = new StringBuilder("insert into ")
                .append(leftTicks())
                .append(tableName)
                .append(rightTicks())
                .append(" (");
        String delimiter = "";
        for (Attribute attribute : entityType.attributes()) {
            if (!(attribute instanceof BasicAttribute)) {
                continue;
            }
            BasicAttribute column = (BasicAttribute) attribute;
            sql.append(delimiter).append(leftTicks()).append(column.columnName()).append(rightTicks());
            columns.add(column);
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
        return new PreparedSqlImpl(sql.toString(), columns, null);
    }

    @NotNull
    protected abstract String rightTicks();

    @NotNull
    protected abstract String leftTicks();

    @Override
    public PreparedSql buildUpdate(@NotNull EntityType entityType, @NotNull List<BasicAttribute> columns) {
        StringBuilder sql = new StringBuilder("update ")
                .append(leftTicks())
                .append(entityType.tableName())
                .append(rightTicks())
                .append(" set ");
        BasicAttribute id = (BasicAttribute) entityType.id();
        String delimiter = "";
        List<BasicAttribute> cms = new ArrayList<>(columns.size() + 1);
        List<BasicAttribute> versions = null;
        for (BasicAttribute column : columns) {
            sql.append(delimiter).append(leftTicks()).append(column.columnName());
            if (column.hasVersion()) {
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
            for (BasicAttribute version : versions) {
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
    public PreparedSql buildDelete(EntityType entity) {
        BasicAttribute id = (BasicAttribute) entity.id();
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
        private List<BasicAttribute> columns;
        private List<BasicAttribute> versionColumns;

        @Override
        public String sql() {
            return this.sql;
        }

        @Override
        public List<BasicAttribute> columns() {
            return this.columns;
        }

        @Override
        public List<BasicAttribute> versionColumns() {
            return versionColumns;
        }
    }
}
