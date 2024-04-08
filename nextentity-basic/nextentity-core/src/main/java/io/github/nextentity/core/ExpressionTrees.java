package io.github.nextentity.core;

import io.github.nextentity.core.TypedExpressions.AbstractTypeExpression;
import io.github.nextentity.core.api.Expression.Column;
import io.github.nextentity.core.api.Expression.Constant;
import io.github.nextentity.core.api.Expression.ExpressionTree;
import io.github.nextentity.core.api.Expression.From.Entity;
import io.github.nextentity.core.api.Expression.From.FromSubQuery;
import io.github.nextentity.core.api.Expression.Operation;
import io.github.nextentity.core.api.Expression.Order;
import io.github.nextentity.core.api.Expression.Selection.EntitySelected;
import io.github.nextentity.core.api.Expression.Selection.MultiSelected;
import io.github.nextentity.core.api.Expression.Selection.ProjectionSelected;
import io.github.nextentity.core.api.Expression.Selection.SingleSelected;
import io.github.nextentity.core.api.Lists;
import io.github.nextentity.core.api.LockModeType;
import io.github.nextentity.core.api.Operator;
import io.github.nextentity.core.api.Slice;
import io.github.nextentity.core.api.SortOrder;
import io.github.nextentity.core.util.Exceptions;
import io.github.nextentity.core.util.Iterators;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Iterator;
import java.util.List;

final class ExpressionTrees {

    public static ExpressionTree newConstant(Object value) {
        return new ConstantImpl(value);
    }

    public static Column newColumn(String[] path) {
        return new ColumnImpl(path);
    }

    public static ExpressionTree newOperation(List<ExpressionTree> operands, Operator operator) {
        return new ExpressionTrees.OperationImpl(operands, operator);
    }

    @EqualsAndHashCode
    static class QueryStructureImpl implements FromSubQuery, Cloneable {

        Selection select;

        From from;

        ExpressionTree where = Expressions.TRUE;

        List<? extends ExpressionTree> groupBy = Lists.of();

        List<? extends Order<?>> orderBy = Lists.of();

        ExpressionTree having = Expressions.TRUE;

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
        public ExpressionTree where() {
            return where;
        }

        @Override
        public List<? extends ExpressionTree> groupBy() {
            return groupBy;
        }

        @Override
        public List<? extends Order<?>> orderBy() {
            return orderBy;
        }

        @Override
        public ExpressionTree having() {
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
        private final ExpressionTree expression;
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
        private final List<? extends ExpressionTree> expressions;
        private final boolean distinct;
    }

    @lombok.Data
    @Accessors(fluent = true)
    static final class SingleSelectedImpl implements SingleSelected {
        private final Class<?> resultType;
        private final ExpressionTree expression;
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
    private static final class ConstantImpl implements Constant, AbstractTypeExpression {
        private final Object value;
    }

    @lombok.Data
    @Accessors(fluent = true)
    private static final class OperationImpl implements Operation, AbstractTypeExpression {
        private final List<? extends ExpressionTree> operands;
        private final Operator operator;
    }

    @lombok.Data
    @Accessors(fluent = true)
    private static final class ColumnImpl implements Column, AbstractTypeExpression {
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
            return new ColumnImpl(strings);
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
            return new ColumnImpl(strings);
        }

        @NotNull
        @Override
        public Iterator<String> iterator() {
            return Iterators.iterate(paths);
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
            return new ColumnImpl(paths);
        }
    }

    private ExpressionTrees() {
    }
}
