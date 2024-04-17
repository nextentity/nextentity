package io.github.nextentity.meta.jpa;

import io.github.nextentity.core.meta.AbstractMetamodel;
import io.github.nextentity.core.meta.Metamodel;
import io.github.nextentity.core.meta.graph.Property;
import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToMany;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.MappedSuperclass;
import jakarta.persistence.OneToMany;
import jakarta.persistence.OneToOne;
import jakarta.persistence.Table;
import jakarta.persistence.Transient;
import jakarta.persistence.Version;
import lombok.extern.slf4j.Slf4j;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.Arrays;
import java.util.List;

@Slf4j
public class JpaMetamodel extends AbstractMetamodel {
    private static final JpaMetamodel JPA_METAMODEL = new JpaMetamodel();
    private final List<Class<? extends Annotation>> JOIN_ANNOTATIONS =
            Arrays.asList(ManyToOne.class, OneToMany.class, ManyToMany.class, OneToOne.class);

    protected JpaMetamodel() {
    }

    public static Metamodel of() {
        return JPA_METAMODEL;
    }

    @Override
    protected String getTableName(Class<?> javaType) {
        String tableName = getTableNameByAnnotation(javaType);
        return tableName != null ? tableName : getTableNameByClassName(javaType);
    }

    protected String getTableNameByClassName(Class<?> javaType) {
        String tableName;
        tableName = camelbackToUnderline(javaType.getSimpleName());
        tableName = unwrapSymbol(tableName);
        return tableName;
    }

    protected String camelbackToUnderline(String simpleName) {
        return simpleName.replaceAll("([a-z])([A-Z])", "$1_$2").toLowerCase();
    }

    protected String getTableNameByAnnotation(Class<?> javaType) {
        Table table = javaType.getAnnotation(Table.class);
        if (table != null && !table.name().isEmpty()) {
            return table.name();
        }
        Entity entity = javaType.getAnnotation(Entity.class);
        if (entity != null && !entity.name().isEmpty()) {
            return entity.name();
        }
        return null;
    }

    @Override
    protected boolean isMarkedId(Property attribute) {
        Id id = getAnnotation(attribute, Id.class);
        return id != null;
    }

    @Override
    protected String getReferencedColumnName(Property attribute) {
        JoinColumn annotation = getAnnotation(attribute, JoinColumn.class);
        String referencedColumnName = null;
        if (annotation != null) {
            referencedColumnName = annotation.referencedColumnName();
        }
        return referencedColumnName;
    }

    @Override
    protected String getJoinColumnName(Property attribute) {
        JoinColumn annotation = getAnnotation(attribute, JoinColumn.class);
        String joinColumnName = null;
        if (annotation != null) {
            joinColumnName = annotation.name();
        }
        return joinColumnName;
    }

    @Override
    protected boolean isVersionField(Property attribute) {
        Version version = getAnnotation(attribute, Version.class);
        if (version != null) {
            Class<?> type = attribute.javaType();
            if (isSupportVersion(type)) {
                return true;
            } else {
                throw new IllegalStateException("not support version type: " + type);
            }
        }
        return false;
    }

    protected boolean isSupportVersion(Class<?> type) {
        return type == long.class || type == Long.class || type == Integer.class || type == int.class;
    }

    @Override
    protected boolean isTransient(Property attribute) {
        return attribute == null
               || attribute.field() == null
               || Modifier.isTransient(attribute.field().getModifiers())
               || Modifier.isStatic(attribute.field().getModifiers())
               || getAnnotation(attribute, Transient.class) != null;
    }

    @Override
    protected boolean isBasicField(Property attribute) {
        for (Class<? extends Annotation> type : JOIN_ANNOTATIONS) {
            if (getAnnotation(attribute, type) != null) {
                return false;
            }
        }
        return true;
    }

    @Override
    protected boolean isAnyToOne(Property attribute) {
        return getAnnotation(attribute, ManyToOne.class) != null || getAnnotation(attribute, OneToOne.class) != null;
    }

    protected String getColumnName(Property attribute) {
        String columnName = getColumnNameByAnnotation(attribute);
        if (columnName == null) {
            columnName = camelbackToUnderline(attribute.name());
        }
        return unwrapSymbol(columnName);
    }

    @Override
    protected Field[] getSuperClassField(Class<?> baseClass, Class<?> superClass) {
        MappedSuperclass mappedSuperclass = superClass.getAnnotation(MappedSuperclass.class);
        if (mappedSuperclass != null) {
            return superClass.getDeclaredFields();
        } else {
            return new Field[0];
        }
    }

    private final static String[][] AROUND_SYMBOL = {
            {"[", "]"},
            {"`", "`"},
            {"\"", "\""}
    };

    protected String unwrapSymbol(String symbol) {
        for (String[] strings : AROUND_SYMBOL) {
            if (symbol.startsWith(strings[0]) && symbol.endsWith(strings[1])) {
                symbol = symbol.substring(1, symbol.length() - 1);
                break;
            }
        }
        return symbol;
    }

    protected String getColumnNameByAnnotation(Property attribute) {
        Column column = getAnnotation(attribute, Column.class);
        if (column != null && !column.name().isEmpty()) {
            return column.name();
        }
        return null;
    }
}
