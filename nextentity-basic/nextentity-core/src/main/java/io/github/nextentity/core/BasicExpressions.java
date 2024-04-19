package io.github.nextentity.core;

import io.github.nextentity.core.Expressions.AbstractTypeExpression;
import io.github.nextentity.core.api.Expression.PathExpression;
import io.github.nextentity.core.api.LockModeType;
import io.github.nextentity.core.api.Operator;
import io.github.nextentity.core.api.Order;
import io.github.nextentity.core.api.Path;
import io.github.nextentity.core.api.Slice;
import io.github.nextentity.core.api.SortOrder;
import io.github.nextentity.core.api.expression.BaseExpression;
import io.github.nextentity.core.api.expression.Empty;
import io.github.nextentity.core.api.expression.EntityPath;
import io.github.nextentity.core.api.expression.Literal;
import io.github.nextentity.core.api.expression.Operation;
import io.github.nextentity.core.api.expression.QueryStructure.From.Entity;
import io.github.nextentity.core.api.expression.QueryStructure.From.FromSubQuery;
import io.github.nextentity.core.api.expression.QueryStructure.Selected.SelectEntity;
import io.github.nextentity.core.meta.BasicAttribute;
import io.github.nextentity.core.meta.EntitySchema;
import io.github.nextentity.core.reflect.schema.Schema;
import io.github.nextentity.core.util.Exceptions;
import io.github.nextentity.core.util.Iterators;
import io.github.nextentity.core.util.Lists;
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
public class BasicExpressions {

    public static final Empty EMPTY = new EmptyExpression();
    public static final Literal TRUE = literal(true);
    public static final Literal FALSE = literal(false);

    public static boolean isNullOrTrue(BaseExpression expression) {
        return expression == null || expression instanceof Empty || BasicExpressions.isTrue(expression);
    }

    public static boolean isTrue(BaseExpression expression) {
        return expression instanceof Literal
               && Boolean.TRUE.equals(((Literal) expression).value());
    }

    public static boolean isFalse(BaseExpression expression) {
        return expression instanceof Literal
               && Boolean.FALSE.equals(((Literal) expression).value());
    }

    public static BaseExpression of(Object value) {
        if (value instanceof BaseExpression) {
            return ((BaseExpression) value);
        } else if (value instanceof Path<?, ?>) {
            return of((Path<?, ?>) value);
        }
        return BasicExpressions.literal(value);
    }

    public static EntityPath of(Path<?, ?> path) {
        String attributeName = attributeName(path);
        return column(attributeName);
    }

    public static String attributeName(Path<?, ?> path) {
        return PathReference.of(path).getFieldName();
    }

    public static EntityPath column(String path) {
        List<String> paths = new ArrayList<>(1);
        paths.add(path);
        return column(paths);
    }

    public static EntityPath column(List<String> paths) {
        Objects.requireNonNull(paths);
        if (paths.getClass() != ArrayList.class) {
            paths = new ArrayList<>(paths);
        }
        return BasicExpressions.newColumn(paths.toArray(String[]::new));
    }

    public static BaseExpression operate(BaseExpression l, Operator o, BaseExpression r) {
        return operate(l, o, Lists.of(r));
    }

    public static BaseExpression operate(BaseExpression l, Operator o) {
        return operate(l, o, Lists.of());
    }

    public static BaseExpression operate(BaseExpression l, Operator o, List<? extends BaseExpression> r) {
        if (o == Operator.NOT
            && l instanceof Operation
            && ((Operation) l).operator() == Operator.NOT) {
            Operation operation = (Operation) l;
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
        List<BaseExpression> operands;
        if (o.isMultivalued() && l instanceof Operation && ((Operation) l).operator() == o) {
            Operation lo = (Operation) l;
            operands = new ArrayList<>(lo.operands().size() + r.size());
            operands.addAll(lo.operands());
        } else {
            operands = new ArrayList<>(r.size() + 1);
            operands.add(l);
        }
        operands.addAll(r);
        return BasicExpressions.newOperation(operands, o);
    }

    public static <T> List<PathExpression<T, ?>> toExpressionList(Collection<Path<T, ?>> paths) {
        return paths.stream()
                .<PathExpression<T, ?>>map(io.github.nextentity.core.util.Paths::get)
                .collect(Collectors.toList());
    }

    public static Literal literal(Object value) {
        return new LiteralExpression(value);
    }

    public static EntityPath newColumn(String[] path) {
        return new EntityPathExpression(path);
    }

    public static BaseExpression newOperation(List<BaseExpression> operands, Operator operator) {
        return new OperationExpression(operands, operator);
    }

    @EqualsAndHashCode
    static class QueryStructureImpl implements FromSubQuery, Cloneable {

        Selected select;

        From from;

        BaseExpression where = BasicExpressions.TRUE;

        List<? extends BaseExpression> groupBy = Lists.of();

        List<? extends Order<?>> orderBy = Lists.of();

        BaseExpression having = BasicExpressions.TRUE;

        Integer offset;

        Integer limit;

        LockModeType lockType = LockModeType.NONE;

        public QueryStructureImpl(Selected select, From from) {
            this.select = select;
            this.from = from;
        }

        public QueryStructureImpl(EntitySchema entityType) {
            this.from = new FromEntity(entityType.type());
            this.select = new SelectEntity().type(entityType.type());
        }

        protected QueryStructureImpl copy() {
            try {
                return (QueryStructureImpl) super.clone();
            } catch (CloneNotSupportedException e) {
                throw Exceptions.sneakyThrow(e);
            }
        }

        @Override
        public Selected select() {
            return select;
        }

        @Override
        public From from() {
            return from;
        }

        @Override
        public BaseExpression where() {
            return where;
        }

        @Override
        public List<? extends BaseExpression> groupBy() {
            return groupBy;
        }

        @Override
        public List<? extends Order<?>> orderBy() {
            return orderBy;
        }

        @Override
        public BaseExpression having() {
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

    }

    @lombok.Data
    @Accessors(fluent = true)
    static final class FromEntity implements Entity {
        private final Class<?> type;
    }

    @lombok.Data
    @Accessors(fluent = true)
    static final class OrderImpl<T> implements Order<T> {
        private final BaseExpression expression;
        private final SortOrder order;
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
    private static final class LiteralExpression implements Literal, AbstractTypeExpression {
        private final Object value;
    }

    @lombok.Data
    @Accessors(fluent = true)
    private static final class OperationExpression implements Operation, AbstractTypeExpression {
        private final List<? extends BaseExpression> operands;
        private final Operator operator;
    }

    private static final class EmptyExpression implements Empty, AbstractTypeExpression {
    }

    @lombok.Data
    @Accessors(fluent = true)
    private static final class EntityPathExpression implements EntityPath, AbstractTypeExpression {
        private final String[] paths;

        @Override
        public int deep() {
            return paths.length;
        }

        @Override
        public String get(int i) {
            return paths[i];
        }

        @Override
        public EntityPath get(String path) {
            String[] strings = new String[deep() + 1];
            System.arraycopy(paths, 0, strings, 0, paths.length);
            strings[deep()] = path;
            return new BasicExpressions.EntityPathExpression(strings);
        }

        @Override
        public EntityPath parent() {
            return sub(deep() - 1);
        }

        @Override
        public EntityPath subLength(int len) {
            if (len == deep()) {
                return this;
            }
            if (len > deep()) {
                throw new IndexOutOfBoundsException();
            }
            return sub(len);
        }

        @Override
        public BasicAttribute toAttribute(EntitySchema entityType) {
            Schema type = entityType;
            for (String s : this) {
                type = ((EntitySchema) type).getAttribute(s);
            }
            return (BasicAttribute) type;
        }

        @Nullable
        private EntityPath sub(int len) {
            if (len <= 0) {
                return null;
            }
            String[] strings = new String[len];
            System.arraycopy(paths, 0, strings, 0, strings.length);
            return new BasicExpressions.EntityPathExpression(strings);
        }

        @NotNull
        @Override
        public Iterator<String> iterator() {
            return Iterators.iterate(paths);
        }

        @Override
        public EntityPath get(EntityPath column) {
            String[] paths = new String[deep() + column.deep()];
            int i = 0;
            for (String s : this) {
                paths[i++] = s;
            }
            for (String s : column) {
                paths[i++] = s;
            }
            return new BasicExpressions.EntityPathExpression(paths);
        }
    }

    private BasicExpressions() {
    }


}
