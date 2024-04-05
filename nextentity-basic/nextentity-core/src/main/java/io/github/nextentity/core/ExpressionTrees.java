package io.github.nextentity.core;

import io.github.nextentity.core.api.Expression;
import io.github.nextentity.core.api.Expression.Column;
import io.github.nextentity.core.api.Expression.Constant;
import io.github.nextentity.core.api.Expression.Operation;
import io.github.nextentity.core.api.From;
import io.github.nextentity.core.api.From.Entity;
import io.github.nextentity.core.api.From.FromSubQuery;
import io.github.nextentity.core.api.Lists;
import io.github.nextentity.core.api.LockModeType;
import io.github.nextentity.core.api.Operator;
import io.github.nextentity.core.api.Order;
import io.github.nextentity.core.api.Selection;
import io.github.nextentity.core.api.Selection.EntitySelected;
import io.github.nextentity.core.api.Selection.MultiSelected;
import io.github.nextentity.core.api.Selection.ProjectionSelected;
import io.github.nextentity.core.api.Selection.SingleSelected;
import io.github.nextentity.core.api.Slice;
import io.github.nextentity.core.api.TypedExpression;
import io.github.nextentity.core.util.Exceptions;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Iterator;
import java.util.List;

final class ExpressionTrees {

    @EqualsAndHashCode
    static class QueryStructureImpl implements FromSubQuery, Cloneable {

        Selection select;

        From from;

        Expression where = Expressions.TRUE;

        List<? extends Expression> groupBy = Lists.of();

        List<? extends Order<?>> orderBy = Lists.of();

        Expression having = Expressions.TRUE;

        List<? extends Column> fetch = Lists.of();

        Integer offset;

        Integer limit;

        LockModeType lockType = LockModeType.NONE;

        public QueryStructureImpl(Selection select, From from) {
            this.select = select;
            this.from = from;
        }

        public QueryStructureImpl(Class<?> from) {
            this.from = new FromEntity(from);
            this.select = new EntitySelectedImpl(from, false);
        }

        protected QueryStructureImpl copy() {
            try {
                return (QueryStructureImpl) super.clone();
            } catch (CloneNotSupportedException e) {
                throw Exceptions.sneakyThrow(e);
            }
        }

        @Override
        public Selection select() {
            return select;
        }

        @Override
        public From from() {
            return from;
        }

        @Override
        public Expression where() {
            return where;
        }

        @Override
        public List<? extends Expression> groupBy() {
            return groupBy;
        }

        @Override
        public List<? extends Order<?>> orderBy() {
            return orderBy;
        }

        @Override
        public Expression having() {
            return having;
        }

        @Override
        public Integer offset() {
            return offset;
        }

        @Override
        public Integer limit() {
            return limit;
        }

        @Override
        public LockModeType lockType() {
            return lockType;
        }

        @Override
        public List<? extends Column> fetch() {
            return fetch;
        }

    }

    @lombok.Data
    @Accessors(fluent = true)
    static final class FromEntity implements Entity {
        private final Class<?> type;
    }

    @lombok.Data
    @Accessors(fluent = true)
    static final class OrderImpl<T> implements Order<T> {
        private final Expression expression;
        private final SortOrder order;
    }

    @lombok.Data
    @Accessors(fluent = true)
    static final class EntitySelectedImpl implements EntitySelected {
        private final Class<?> resultType;
        private final boolean distinct;
    }

    @lombok.Data
    @Accessors(fluent = true)
    static final class ProjectionSelectedImpl implements ProjectionSelected {
        private final Class<?> resultType;
        private final boolean distinct;
    }

    @lombok.Data
    @Accessors(fluent = true)
    static final class MultiSelectedImpl implements MultiSelected {
        private final List<? extends Expression> expressions;
        private final boolean distinct;
    }

    @lombok.Data
    @Accessors(fluent = true)
    static final class SingleSelectedImpl implements SingleSelected {
        private final Class<?> resultType;
        private final Expression expression;
        private final boolean distinct;
    }

    @lombok.Data
    @Accessors(fluent = true)
    static final class SliceImpl<T> implements Slice<T> {
        private final List<T> data;
        private final long total;
        private final int offset;
        private final int limit;
    }

    @lombok.Data
    @Accessors(fluent = true)
    static final class ConstantImpl<T, R> implements Constant, TypedExpression<T, R> {
        private final Object value;
    }

    @lombok.Data
    @Accessors(fluent = true)
    static final class OperationImpl<T, R> implements Operation, TypedExpression<T, R> {
        private final List<? extends Expression> operands;
        private final Operator operator;
    }

    @lombok.Data
    @Accessors(fluent = true)
    static final class ColumnImpl<T, R> implements Column, TypedExpression<T, R> {
        private final String[] paths;

        @Override
        public int size() {
            return paths.length;
        }

        @Override
        public String get(int i) {
            return paths[i];
        }

        @Override
        public Column get(String path) {
            String[] strings = new String[size() + 1];
            System.arraycopy(paths, 0, strings, 0, paths.length);
            strings[size()] = path;
            return new ColumnImpl<>(strings);
        }

        @Override
        public Column parent() {
            return sub(size() - 1);
        }

        @Override
        public Column subLength(int len) {
            if (len == size()) {
                return this;
            }
            if (len > size()) {
                throw new IndexOutOfBoundsException();
            }
            return sub(len);
        }

        @Nullable
        private Column sub(int len) {
            if (len <= 0) {
                return null;
            }
            String[] strings = new String[len];
            System.arraycopy(paths, 0, strings, 0, strings.length);
            return new ColumnImpl<>(strings);
        }

        @NotNull
        @Override
        public Iterator<String> iterator() {
            return new ArrayIterator<>(paths);
        }

        @Override
        public Column get(Column column) {
            String[] paths = new String[size() + column.size()];
            int i = 0;
            for (String s : this) {
                paths[i++] = s;
            }
            for (String s : column) {
                paths[i++] = s;
            }
            return new ColumnImpl<>(paths);
        }
    }

    private ExpressionTrees() {
    }
}