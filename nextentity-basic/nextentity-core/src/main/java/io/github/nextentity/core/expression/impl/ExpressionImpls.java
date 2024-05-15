package io.github.nextentity.core.expression.impl;

import io.github.nextentity.api.Expression;
import io.github.nextentity.api.Path;
import io.github.nextentity.api.TypedExpression;
import io.github.nextentity.api.model.LockModeType;
import io.github.nextentity.api.model.Order;
import io.github.nextentity.api.model.Slice;
import io.github.nextentity.core.PathReference;
import io.github.nextentity.core.TypeCastUtil;
import io.github.nextentity.core.expression.EntityPath;
import io.github.nextentity.core.expression.Literal;
import io.github.nextentity.core.expression.Operation;
import io.github.nextentity.core.expression.Operator;
import io.github.nextentity.core.expression.QueryStructure;
import io.github.nextentity.core.meta.EntityType;
import io.github.nextentity.core.util.EmptyArrays;
import io.github.nextentity.core.util.ImmutableList;
import io.github.nextentity.core.util.ImmutableList.Builder;
import io.github.nextentity.core.util.Paths;
import lombok.experimental.Accessors;
import lombok.extern.slf4j.Slf4j;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.List;

@Slf4j
public class ExpressionImpls {

    public static final Expression EMPTY = EmptyExpression.EMPTY;
    public static final Literal TRUE = LiteralImpl.TRUE;
    public static final Literal FALSE = LiteralImpl.FALSE;

    public static boolean isNullOrTrue(Expression expression) {
        return expression == null || expression == EMPTY || ExpressionImpls.isTrue(expression);
    }

    public static boolean isTrue(Expression expression) {
        return expression instanceof Literal
               && Boolean.TRUE.equals(((Literal) expression).value());
    }

    public static boolean isFalse(Expression expression) {
        return expression instanceof Literal
               && Boolean.FALSE.equals(((Literal) expression).value());
    }

    public static Expression of(Object value) {
        if (value instanceof Expression) {
            return ((Expression) value);
        } else if (value instanceof Path<?, ?>) {
            return of((Path<?, ?>) value);
        }
        return ExpressionImpls.literal(value);
    }

    public static EntityPath of(Path<?, ?> path) {
        String attributeName = attributeName(path);
        return column(attributeName);
    }

    public static String attributeName(Path<?, ?> path) {
        return PathReference.of(path).getFieldName();
    }

    public static EntityPath column(String path) {
        return ExpressionImpls.newColumn(new String[]{path});
    }

    public static EntityPath column(List<String> paths) {
        return ExpressionImpls.newColumn(paths.toArray(EmptyArrays.STRING));
    }

    public static Expression operate(Expression l, Operator o, Expression r) {
        return operate(l, o, ImmutableList.of(r));
    }

    public static Expression operate(Expression l, Operator o) {
        return operate(l, o, ImmutableList.of());
    }

    public static Expression operate(Expression l, Operator o, List<? extends Expression> r) {
        if (o == Operator.NOT
            && l instanceof Operation
            && ((Operation) l).operator() == Operator.NOT) {
            //noinspection PatternVariableCanBeUsed
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
        Builder<Expression> builder;
        if (o.isMultivalued() && l instanceof Operation && ((Operation) l).operator() == o) {
            //noinspection PatternVariableCanBeUsed
            Operation lo = (Operation) l;
            List<? extends Expression> operands = lo.operands();
            builder = new Builder<>(operands.size() + r.size());
            addAll(builder, operands);
        } else {
            builder = new Builder<>(r.size() + 1);
            if (l != EMPTY) {
                builder.add(l);
            }
        }
        addAll(builder, r);
        if (builder.isEmpty()) {
            return EMPTY;
        }
        return ExpressionImpls.newOperation(builder.build(), o);
    }

    private static void addAll(Builder<Expression> builder, List<? extends Expression> expressions) {
        for (Expression expression : expressions) {
            if (expression != EMPTY) {
                builder.add(expression);
            }
        }
    }

    public static <T> List<TypedExpression.PathExpression<T, ?>> toExpressionList(Collection<Path<T, ?>> paths) {
        return paths.stream()
                .<TypedExpression.PathExpression<T, ?>>map(Paths::get)
                .collect(ImmutableList.collector(paths.size()));
    }

    public static Literal literal(Object value) {
        return new LiteralImpl(value);
    }

    public static EntityPath newColumn(String[] path) {
        return new EntityPathImpl(path);
    }

    public static Expression newOperation(List<? extends Expression> operands, Operator operator) {
        return new OperationImpl(operands, operator);
    }

    public static QueryStructure queryStructure(QueryStructure.Selected select,
                                                QueryStructure.From from,
                                                Expression where,
                                                List<? extends Expression> groupBy,
                                                List<? extends Order<?>> orderBy,
                                                Expression having,
                                                Integer offset,
                                                Integer limit,
                                                LockModeType lockType) {
        return new QueryStructureImpl(select, from, where, groupBy, orderBy, having, offset, limit, lockType);
    }

    public static QueryStructure queryStructure(EntityType entityType) {
        return new QueryStructureImpl(entityType);
    }

    public static QueryStructure queryStructure(Class<?> entityType) {
        return new QueryStructureImpl(entityType);
    }

    public static QueryStructure queryStructure(QueryStructure.Selected select, QueryStructure.From from) {
        return new QueryStructureImpl(select, from);
    }

    public static QueryStructure where(QueryStructure structure, Expression where) {
        return new QueryStructureImpl(
                structure.select(),
                structure.from(),
                where,
                structure.groupBy(),
                structure.orderBy(),
                structure.having(),
                structure.offset(),
                structure.limit(),
                structure.lockType()
        );
    }

    @NotNull
    public static QueryStructure update(QueryStructure structure, int offset, int limit, LockModeType lockModeType) {
        return new QueryStructureImpl(
                structure.select(),
                structure.from(),
                structure.where(),
                structure.groupBy(),
                structure.orderBy(),
                structure.having(),
                offset,
                limit,
                lockModeType
        );
    }

    public static QueryStructure.From from(QueryStructure queryStructure) {
        if (queryStructure instanceof QueryStructure.From) {
            return (QueryStructure.From) queryStructure;
        } else {
            return new QueryStructureImpl(queryStructure);
        }
    }


    @lombok.Data
    @Accessors(fluent = true)
    public static final class SliceImpl<T> implements Slice<T> {
        private final List<T> data;
        private final long total;
        private final int offset;
        private final int limit;
    }

    private ExpressionImpls() {
    }

    static <T extends TypedExpression<?, ?>> T toTypedExpression(Expression expression) {
        return TypeCastUtil.unsafeCast(expression);
    }
}
