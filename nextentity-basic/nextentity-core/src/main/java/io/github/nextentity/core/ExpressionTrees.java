package io.github.nextentity.core;

import io.github.nextentity.core.Expressions.AbstractTypeExpression;
import io.github.nextentity.core.api.Expression.PathExpression;
import io.github.nextentity.core.api.ExpressionTree;
import io.github.nextentity.core.api.ExpressionTree.Column;
import io.github.nextentity.core.api.ExpressionTree.ExpressionNode;
import io.github.nextentity.core.api.ExpressionTree.Literal;
import io.github.nextentity.core.api.ExpressionTree.Operation;
import io.github.nextentity.core.api.ExpressionTree.QueryStructure.From.Entity;
import io.github.nextentity.core.api.ExpressionTree.QueryStructure.From.FromSubQuery;
import io.github.nextentity.core.api.ExpressionTree.QueryStructure.Order;
import io.github.nextentity.core.api.ExpressionTree.QueryStructure.Selection.EntitySelected;
import io.github.nextentity.core.api.ExpressionTree.QueryStructure.Selection.MultiSelected;
import io.github.nextentity.core.api.ExpressionTree.QueryStructure.Selection.ProjectionSelected;
import io.github.nextentity.core.api.ExpressionTree.QueryStructure.Selection.SingleSelected;
import io.github.nextentity.core.util.Lists;
import io.github.nextentity.core.api.LockModeType;
import io.github.nextentity.core.api.Operator;
import io.github.nextentity.core.api.Path;
import io.github.nextentity.core.api.Slice;
import io.github.nextentity.core.api.SortOrder;
import io.github.nextentity.core.util.Exceptions;
import io.github.nextentity.core.util.Iterators;
import io.github.nextentity.core.util.Paths;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;
import lombok.extern.slf4j.Slf4j;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@Slf4j
@SuppressWarnings("PatternVariableCanBeUsed")
public class ExpressionTrees {

    public static final ExpressionNode TRUE = literal(true);
    public static final ExpressionNode FALSE = literal(false);

    public static boolean isNullOrTrue(ExpressionTree expression) {
        return expression == null || ExpressionTrees.isTrue(expression);
    }

    public static boolean isTrue(ExpressionTree expression) {
        ExpressionNode tree = expression.rootNode();
        return tree instanceof Literal
               && Boolean.TRUE.equals(((Literal) tree).value());
    }

    public static boolean isFalse(ExpressionTree expression) {
        ExpressionNode tree = expression.rootNode();
        return tree instanceof Literal
               && Boolean.FALSE.equals(((Literal) tree).value());
    }

    public static ExpressionNode of(Object value) {
        if (value instanceof ExpressionTree) {
            return ((ExpressionTree) value).rootNode();
        } else if (value instanceof Path<?, ?>) {
            return of((Path<?, ?>) value);
        }
        return ExpressionTrees.literal(value);
    }

    public static Column of(Path<?, ?> path) {
        String property = columnName(path);
        return column(property);
    }

    public static String columnName(Path<?, ?> path) {
        return PathReference.of(path).getPropertyName();
    }

    public static Column column(String path) {
        List<String> paths = new ArrayList<>(1);
        paths.add(path);
        return column(paths);
    }

    public static Column column(List<String> paths) {
        Objects.requireNonNull(paths);
        if (paths.getClass() != ArrayList.class) {
            paths = new ArrayList<>(paths);
        }
        return ExpressionTrees.newColumn(paths.toArray(String[]::new));
    }

    public static ExpressionNode operate(ExpressionTree l, Operator o, ExpressionTree r) {
        return operate(l, o, Lists.of(r));
    }

    public static ExpressionNode operate(ExpressionTree l, Operator o) {
        return operate(l, o, Lists.of());
    }

    public static ExpressionNode operate(ExpressionTree l, Operator o, List<? extends ExpressionTree> r) {
        ExpressionNode tree = l.rootNode();
        if (o == Operator.NOT
            && tree instanceof Operation
            && ((Operation) tree).operator() == Operator.NOT) {
            Operation operation = (Operation) tree;
            return operation.firstOperand();
        }
        if (o == Operator.NOT) {
            if (isTrue(l)) {
                return FALSE;
            } else if (isFalse(l)) {
                return TRUE;
            }
        }
        if (o == Operator.IN && r.isEmpty()) {
            log.warn("operator `in` right operands is empty");
            return FALSE;
        }
        List<ExpressionNode> operands;
        if (o.isMultivalued() && tree instanceof Operation && ((Operation) tree).operator() == o) {
            Operation lo = (Operation) tree;
            operands = new ArrayList<>(lo.operands().size() + r.size());
            operands.addAll(lo.operands());
        } else {
            operands = new ArrayList<>(r.size() + 1);
            operands.add(tree.rootNode());
        }
        for (ExpressionTree expression : r) {
            operands.add(expression.rootNode());
        }
        return ExpressionTrees.newOperation(operands, o);
    }

    public static <T> List<PathExpression<T, ?>> toExpressionList(Collection<Path<T, ?>> paths) {
        return paths.stream()
                .<PathExpression<T, ?>>map(Paths::get)
                .collect(Collectors.toList());
    }

    public static ExpressionNode literal(Object value) {
        return new LiteralImpl(value);
    }

    public static Column newColumn(String[] path) {
        return new ColumnImpl(path);
    }

    public static ExpressionNode newOperation(List<ExpressionNode> operands, Operator operator) {
        return new ExpressionTrees.OperationImpl(operands, operator);
    }

    @EqualsAndHashCode
    static class QueryStructureImpl implements FromSubQuery, Cloneable {

        Selection select;

        From from;

        ExpressionNode where = ExpressionTrees.TRUE;

        List<? extends ExpressionNode> groupBy = Lists.of();

        List<? extends Order<?>> orderBy = Lists.of();

        ExpressionNode having = ExpressionTrees.TRUE;

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
        public ExpressionNode where() {
            return where;
        }

        @Override
        public List<? extends ExpressionNode> groupBy() {
            return groupBy;
        }

        @Override
        public List<? extends Order<?>> orderBy() {
            return orderBy;
        }

        @Override
        public ExpressionNode having() {
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
        private final ExpressionNode expression;
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
        private final List<? extends ExpressionNode> expressions;
        private final boolean distinct;
    }

    @lombok.Data
    @Accessors(fluent = true)
    static final class SingleSelectedImpl implements SingleSelected {
        private final Class<?> resultType;
        private final ExpressionNode expression;
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
    private static final class LiteralImpl implements Literal, AbstractTypeExpression {
        private final Object value;
    }

    @lombok.Data
    @Accessors(fluent = true)
    private static final class OperationImpl implements Operation, AbstractTypeExpression {
        private final List<? extends ExpressionNode> operands;
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
