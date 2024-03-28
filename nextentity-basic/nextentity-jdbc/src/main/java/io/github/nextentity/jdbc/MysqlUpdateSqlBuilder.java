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
public class MysqlUpdateSqlBuilder implements JdbcUpdateSqlBuilder {

    @Override
    public PreparedSql buildInsert(@NotNull EntityType entityType) {
        String tableName = entityType.tableName();
        List<BasicAttribute> columns = new ArrayList<>();
        StringBuilder sql = new StringBuilder("insert into `")
                .append(tableName).append("` (");
        String delimiter = "";
        for (Attribute attribute : entityType.attributes()) {
            if (!(attribute instanceof BasicAttribute)) {
                continue;
            }
            BasicAttribute column = (BasicAttribute) attribute;
            sql.append(delimiter).append("`").append(column.columnName()).append("`");
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

    @Override
    public PreparedSql buildUpdate(@NotNull EntityType entityType, @NotNull List<BasicAttribute> columns) {
        StringBuilder sql = new StringBuilder("update `").append(entityType.tableName()).append("` set ");
        BasicAttribute id = (BasicAttribute) entityType.id();
        String delimiter = "";
        List<BasicAttribute> cms = new ArrayList<>(columns.size() + 1);
        List<BasicAttribute> versions = null;
        for (BasicAttribute column : columns) {
            sql.append(delimiter).append("`").append(column.columnName());
            if (column.hasVersion()) {
                sql.append("`=`").append(column.columnName()).append("`+1");
                versions = versions == null ? new ArrayList<>(1) : versions;
                versions.add(column);
            } else {
                sql.append("`=?");
                cms.add(column);
            }
            delimiter = ",";
        }
        sql.append(" where `").append(id.columnName()).append("`=?");
        cms.add(id);
        if (versions != null) {
            for (BasicAttribute version : versions) {
                sql.append(" and `").append(version.columnName()).append("`=?");
                cms.add(version);
            }
        }
        return new PreparedSqlImpl(sql.toString(), cms, versions);
    }

    @Override
    public PreparedSql buildDelete(EntityType entity) {
        BasicAttribute id = (BasicAttribute) entity.id();
        String sql = "delete from `" + entity.tableName() + "` where `" + id.columnName() + "`=?";
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
