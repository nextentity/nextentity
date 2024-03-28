package io.github.nextentity.core;

import io.github.nextentity.core.api.Column;
import io.github.nextentity.core.api.Expression;
import io.github.nextentity.core.api.ExpressionOperator;
import io.github.nextentity.core.api.ExpressionOperator.AndOperator;
import io.github.nextentity.core.api.ExpressionOperator.ComparableOperator;
import io.github.nextentity.core.api.ExpressionOperator.NumberOperator;
import io.github.nextentity.core.api.ExpressionOperator.OrOperator;
import io.github.nextentity.core.api.ExpressionOperator.PathOperator;
import io.github.nextentity.core.api.ExpressionOperator.StringOperator;
import io.github.nextentity.core.api.Lists;
import io.github.nextentity.core.api.Operation;
import io.github.nextentity.core.api.Operator;
import io.github.nextentity.core.api.Order;
import io.github.nextentity.core.api.Order.SortOrder;
import io.github.nextentity.core.api.Path;
import io.github.nextentity.core.api.Path.BooleanPath;
import io.github.nextentity.core.api.Path.ComparablePath;
import io.github.nextentity.core.api.Path.NumberPath;
import io.github.nextentity.core.api.Path.StringPath;
import io.github.nextentity.core.api.Root;
import io.github.nextentity.core.api.TypedExpression;
import io.github.nextentity.core.api.TypedExpression.BasicExpression;
import io.github.nextentity.core.api.TypedExpression.BooleanExpression;
import io.github.nextentity.core.api.TypedExpression.BooleanPathExpression;
import io.github.nextentity.core.api.TypedExpression.ComparableExpression;
import io.github.nextentity.core.api.TypedExpression.ComparablePathExpression;
import io.github.nextentity.core.api.TypedExpression.EntityPathExpression;
import io.github.nextentity.core.api.TypedExpression.NumberExpression;
import io.github.nextentity.core.api.TypedExpression.NumberPathExpression;
import io.github.nextentity.core.api.TypedExpression.PathExpression;
import io.github.nextentity.core.api.TypedExpression.Predicate;
import io.github.nextentity.core.api.TypedExpression.StringExpression;
import io.github.nextentity.core.api.TypedExpression.StringPathExpression;
import io.github.nextentity.core.QueryStructures.OrderImpl;
import lombok.experimental.Accessors;
import org.jetbrains.annotations.NotNull;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

public class TypedExpressions {

    public static <T, U> TypedExpression<T, U> of(Expression expression) {
        return TypeCastUtil.cast(expression);
    }

    public static <T, U> TypedExpression<T, U> of(U value) {
        return of(Expressions.of(value));
    }

    public static <T, U> List<TypedExpression<T, U>> ofList(U[] values) {
        return Arrays.stream(values)
                .map(TypedExpressions::<T, U>of)
                .collect(Collectors.toList());
    }

    public static <T, U> List<TypedExpression<T, U>> ofList(Iterable<? extends U> value) {
        return StreamSupport.stream(value.spliterator(), false)
                .map(TypedExpressions::<T, U>of)
                .collect(Collectors.toList());
    }

    public static <T, U> TypedExpression<T, U> ofTrue() {
        return of(Expressions.TRUE);
    }

    public static <T, R> PathExpression<T, R> ofPath(Column column) {
        return new _Basic<>((Operation) null, column);
    }

    public static <T, R> EntityPathExpression<T, R> ofEntity(Column column) {
        return new _Basic<>((Operation) null, column);
    }

    public static <T> StringPathExpression<T> ofString(Column column) {
        return new _String<>((Operation) null, column);
    }

    public static <T> BooleanPathExpression<T> ofBoolean(Column column) {
        return new _Boolean<>((Operation) null, column);
    }

    public static <T, U extends Number & Comparable<U>> NumberPathExpression<T, U> ofNumber(Column column) {
        return new _Number<>((Operation) null, column);
    }

    public static <T, U extends Comparable<U>> ComparablePathExpression<T, U> ofComparable(Column column) {
        return new _Comparable<>((Operation) null, column);
    }

    public static <T, R> BasicExpression<T, R> ofBasic(Expression expression) {
        return new _Basic<>((Operation) null, expression);
    }

    public static <T> StringExpression<T> ofString(Expression expression) {
        return new _String<>((Operation) null, expression);
    }

    public static <T> BooleanExpression<T> ofBoolean(Expression expression) {
        return new _Boolean<>((Operation) null, expression);
    }

    public static <T, U extends Number & Comparable<U>> NumberExpression<T, U> ofNumber(Expression expression) {
        return new _Number<>((Operation) null, expression);
    }

    public static <T, U extends Comparable<U>> ComparableExpression<T, U> ofComparable(Expression expression) {
        return new _Comparable<>((Operation) null, expression);
    }

    static class _Basic<T, U> implements BasicExpression<T, U>, EntityPathExpression<T, U> {
        protected final Operation operation;
        protected final Expression operand;

        public _Basic(Operation operation, Expression operand) {
            this.operation = operation;
            this.operand = operand;
        }

        public _Basic(_Basic<?, ?> operation, Expression operand) {
            this(operation == null ? null : operation.operation, operand);
        }

        protected Expression operate(Operator operator, Object value) {
            return operate(operator, of(value));
        }

        protected Expression operate(Operator operator, TypedExpression<T, ?> expression) {
            return operate(operator, Lists.of(expression));
        }

        protected Expression operate(Operator operator, Iterable<? extends TypedExpression<T, ?>> expressions) {
            List<Expression> args = StreamSupport.stream(expressions.spliterator(), false)
                    .map(TypedExpression::expression)
                    .collect(Collectors.toList());
            return Expressions.operate(this.operand, operator, args);
        }

        @Override
        public BooleanExpression<T> eq(U value) {
            return eq(of(value));
        }

        @Override
        public BooleanExpression<T> eqIfNotNull(U value) {
            return value == null ? rollback() : eq(value);
        }

        @Override
        public BooleanExpression<T> eq(TypedExpression<T, U> value) {
            Expression operate = operate(Operator.EQ, value);
            return new _Boolean<>(this, operate);
        }

        @Override
        public BooleanExpression<T> ne(U value) {
            return ne(of(value));
        }

        @Override
        public BooleanExpression<T> neIfNotNull(U value) {
            return value == null ? rollback() : ne(value);
        }

        @NotNull
        protected TypedExpressions._Boolean<T> rollback() {
            return new _Boolean<>(operation, null);
        }

        @Override
        public BooleanExpression<T> ne(TypedExpression<T, U> value) {
            Expression operate = operate(Operator.NE, value);
            return new _Boolean<>(this, operate);
        }

        @SafeVarargs
        @Override
        public final BooleanExpression<T> in(U... values) {
            return in(ofList(values));
        }

        @Override
        public BooleanExpression<T> in(@NotNull List<? extends TypedExpression<T, U>> values) {
            Expression operate = operate(Operator.IN, values);
            return new _Boolean<>(this, operate);
        }

        @Override
        public BooleanExpression<T> in(@NotNull Collection<? extends U> values) {
            return in(ofList(values));
        }

        @SafeVarargs
        @Override
        public final BooleanExpression<T> notIn(U... values) {
            return notIn(ofList(values));
        }

        @Override
        public BooleanExpression<T> notIn(@NotNull List<? extends TypedExpression<T, U>> values) {
            List<Expression> expressions = values.stream()
                    .map(TypedExpression::expression)
                    .collect(Collectors.toList());
            Expression operate = Expressions.operate(operand, Operator.IN, expressions);
            operate = Expressions.operate(operate, Operator.NOT);
            return new _Boolean<>(this, operate);
        }

        @Override
        public BooleanExpression<T> notIn(@NotNull Collection<? extends U> values) {
            return notIn(ofList(values));
        }

        @Override
        public BooleanExpression<T> isNull() {
            Expression operate = Expressions.operate(operand, Operator.IS_NULL);
            return new _Boolean<>(this, operate);
        }

        @Override
        public NumberExpression<T, Long> count() {
            Expression operate = Expressions.operate(operand, Operator.COUNT);
            return new _Number<>(this, operate);
        }

        @Override
        public BooleanExpression<T> isNotNull() {
            Expression operate = Expressions.operate(operand, Operator.IS_NOT_NULL);
            return new _Boolean<>(this, operate);
        }

        @Override
        public Root<T> root() {
            return RootImpl.of();
        }

        @Override
        public Expression expression() {
            if (operand == null) {
                return operation;
            } else if (operation == null) {
                return operand;
            } else {
                Operator operator = operation.operator();
                if (operator == Operator.OR || operator == Operator.AND) {
                    return Expressions.operate(operation, operator, operand);
                } else {
                    throw new IllegalStateException();
                }
            }
        }

        private <R> Column getPath(Path<U, R> path) {
            if (operand instanceof Column) {
                return Expressions.concat((Column) operand, path);
            }
            throw new IllegalStateException();
        }

        @Override
        public <R> EntityPathExpression<T, R> get(Path<U, R> path) {
            return new _Basic<>(this, getPath(path));
        }

        @Override
        public StringPathExpression<T> get(StringPath<U> path) {
            return new _String<>(this, getPath(path));
        }

        @Override
        public <R extends Number & Comparable<R>> NumberPathExpression<T, R> get(NumberPath<U, R> path) {
            return new _Number<>(this, getPath(path));
        }

        @Override
        public <R extends Comparable<R>> ComparablePathExpression<T, R> get(ComparablePath<U, R> path) {
            return new _Comparable<>(this, getPath(path));
        }

        @Override
        public BooleanPathExpression<T> get(BooleanPath<U> path) {
            return new _Boolean<>(this, getPath(path));
        }

        @Override
        public <R> PathExpression<T, R> get(PathExpression<U, R> path) {
            Column column = getColumn(path);
            return ofPath(column);
        }

        private <R> Column getColumn(PathExpression<U, R> path) {
            Column pre = (Column) expression();
            Column next = (Column) path.expression();
            return pre.get(next);
        }

        @Override
        public StringPathExpression<T> get(StringPathExpression<U> path) {
            return ofString(getColumn(path));
        }

        @Override
        public <R extends Number & Comparable<R>> NumberPathExpression<T, R> get(NumberPathExpression<U, R> path) {
            return ofNumber(getColumn(path));
        }

        @Override
        public <R extends Comparable<R>> ComparablePathExpression<T, R> get(ComparablePathExpression<U, R> path) {
            return ofComparable(getColumn(path));
        }

        @Override
        public BooleanPathExpression<T> get(BooleanPathExpression<U> path) {
            return ofBoolean(getColumn(path));
        }

        protected Operation and() {
            return updateOperation(Operator.AND);
        }

        protected Operation or() {
            return updateOperation(Operator.OR);
        }

        protected Operation updateOperation(Operator operator) {
            if (operand == null) {
                return operation;
            }
            if (operation == null) {
                return (Operation) Expressions.operate(operand, operator);
            }
            if (operation.operator() != operator) {
                throw new IllegalStateException();
            }
            return (Operation) Expressions.operate(operation, operator, operand);
        }

    }

    static class _Boolean<T>
            extends _Comparable<T, Boolean>
            implements BooleanPathExpression<T> {

        public _Boolean(_Basic<?, ?> origin, Expression operand) {
            super(origin, operand);
        }

        public _Boolean(Operation operation, Expression operand) {
            super(operation, operand);
        }

        @Override
        public BooleanExpression<T> not() {
            return new _Boolean<>(this, Expressions.operate(operand, Operator.NOT));
        }

        @Override
        public <R> PathOperator<T, R, OrOperator<T>> or(Path<T, R> path) {
            PathExpression<T, R> expression = new _Basic<>(or(), Expressions.of(path));
            return ExpressionOperators.ofPath(expression, this::newOrOperator);
        }

        @NotNull
        OrOperator<T> newOrOperator(BasicExpression<?, ?> expression) {
            if (expression instanceof OrOperator) {
                return TypeCastUtil.unsafeCast(expression);
            }
            _Basic<?, ?> expr = (_Basic<?, ?>) expression;
            return new _Boolean<>(expr.operation, expr.operand);
        }

        @NotNull
        ExpressionOperator.AndOperator<T> newAndOperator(BasicExpression<?, ?> expression) {
            if (expression instanceof ExpressionOperator.AndOperator) {
                return TypeCastUtil.unsafeCast(expression);
            }
            _Basic<?, ?> expr = (_Basic<?, ?>) expression;
            return new _Boolean<>(expr.operation, expr.operand);
        }

        @Override
        public <R extends Comparable<R>> ComparableOperator<T, R, OrOperator<T>> or(ComparablePath<T, R> path) {
            ComparableExpression<T, R> expression = new _Comparable<>(or(), Expressions.of(path));
            return ExpressionOperators.ofComparable(expression, this::newOrOperator);
        }

        @Override
        public <R extends Number & Comparable<R>> NumberOperator<T, R, OrOperator<T>> or(NumberPath<T, R> path) {
            NumberExpression<T, R> expression = new _Number<>(or(), Expressions.of(path));
            return ExpressionOperators.ofNumber(expression, this::newOrOperator);
        }

        @Override
        public OrOperator<T> or(BooleanPath<T> path) {
            return new _Boolean<>(or(), Expressions.of(path));
        }

        @Override
        public StringOperator<T, ? extends OrOperator<T>> or(StringPath<T> path) {
            StringExpression<T> expression = new _String<>(or(), Expressions.of(path));
            return ExpressionOperators.ofString(expression, this::newOrOperator);
        }

        @Override
        public OrOperator<T> or(TypedExpression<T, Boolean> expression) {
            return new _Boolean<>(or(), expression.expression());
        }

        @Override
        public OrOperator<T> or(List<? extends TypedExpression<T, Boolean>> expressions) {
            if (expressions.isEmpty()) {
                return this;
            }
            List<Expression> sub = expressions
                    .subList(0, expressions.size() - 1)
                    .stream().map(TypedExpression::expression)
                    .collect(Collectors.toList());
            Operation operation = (Operation) Expressions.operate(or(), Operator.OR, sub);
            Expression last = expressions.get(expressions.size() - 1).expression();
            return new _Boolean<>(operation, last);
        }

        @Override
        public <R> PathOperator<T, R, AndOperator<T>> and(Path<T, R> path) {
            PathExpression<T, R> expression = new _Basic<>(and(), Expressions.of(path));
            return ExpressionOperators.ofPath(expression, this::newAndOperator);
        }

        @Override
        public <R extends Comparable<R>> ComparableOperator<T, R, AndOperator<T>> and(ComparablePath<T, R> path) {
            ComparableExpression<T, R> expression = new _Comparable<>(and(), Expressions.of(path));
            return ExpressionOperators.ofComparable(expression, this::newAndOperator);
        }

        @Override
        public <R extends Number & Comparable<R>> NumberOperator<T, R, AndOperator<T>> and(NumberPath<T, R> path) {
            NumberExpression<T, R> expression = new _Number<>(and(), Expressions.of(path));
            return ExpressionOperators.ofNumber(expression, this::newAndOperator);
        }

        @Override
        public AndOperator<T> and(BooleanPath<T> path) {
            BooleanExpression<T> expression = new _Boolean<>(and(), Expressions.of(path));
            return new _Boolean<>(and(), expression.expression());
        }

        @Override
        public StringOperator<T, AndOperator<T>> and(StringPath<T> path) {
            StringExpression<T> expression = new _String<>(and(), Expressions.of(path));
            return ExpressionOperators.ofString(expression, this::newAndOperator);
        }

        @Override
        public AndOperator<T> and(TypedExpression<T, Boolean> expression) {
            return new _Boolean<>(and(), expression.expression());
        }

        @Override
        public AndOperator<T> and(List<? extends TypedExpression<T, Boolean>> expressions) {
            BooleanExpression<T> expr = this;
            if (!expressions.isEmpty()) {
                List<Expression> sub = expressions
                        .subList(0, expressions.size() - 1)
                        .stream().map(TypedExpression::expression)
                        .collect(Collectors.toList());
                Operation operation = (Operation) Expressions.operate(and(), Operator.AND, sub);
                Expression last = expressions.get(expressions.size() - 1).expression();
                expr = new _Boolean<>(operation, last);
            }
            return expr;
        }

        @Override
        public Predicate<T> then() {
            return new _Predicate<>(expression());
        }
    }

    static class _Comparable<T, U extends Comparable<U>>
            extends _Basic<T, U>
            implements ComparablePathExpression<T, U> {

        public _Comparable(_Basic<?, ?> origin, Expression operand) {
            super(origin, operand);
        }

        public _Comparable(Operation operation, Expression operand) {
            super(operation, operand);
        }

        @Override
        public BooleanExpression<T> ge(TypedExpression<T, U> expression) {
            return new _Boolean<>(this, operate(Operator.GE, expression));
        }

        @Override
        public BooleanExpression<T> gt(TypedExpression<T, U> expression) {
            return new _Boolean<>(this, operate(Operator.GT, expression));
        }

        @Override
        public BooleanExpression<T> le(TypedExpression<T, U> expression) {
            return new _Boolean<>(this, operate(Operator.LE, expression));
        }

        @Override
        public BooleanExpression<T> lt(TypedExpression<T, U> expression) {
            return new _Boolean<>(this, operate(Operator.LT, expression));
        }

        @Override
        public BooleanExpression<T> between(TypedExpression<T, U> l, TypedExpression<T, U> r) {
            return new _Boolean<>(this, operate(Operator.BETWEEN, Lists.of(l, r)));
        }

        @Override
        public BooleanExpression<T> notBetween(TypedExpression<T, U> l, TypedExpression<T, U> r) {
            Expression operate = operate(Operator.BETWEEN, Lists.of(l, r));
            operate = Expressions.operate(operate, Operator.NOT);
            return new _Boolean<>(this, operate);
        }

        @Override
        public Order<T> sort(SortOrder order) {
            return new OrderImpl<>(operand, order);
        }
    }

    static class _Number<T, U extends Number & Comparable<U>>
            extends _Comparable<T, U>
            implements NumberPathExpression<T, U> {

        public _Number(_Basic<?, ?> origin, Expression operand) {
            super(origin, operand);
        }

        public _Number(Operation operation, Expression operand) {
            super(operation, operand);
        }

        @Override
        public NumberExpression<T, U> add(TypedExpression<T, U> expression) {
            return new _Number<>(this, operate(Operator.ADD, expression));
        }

        @Override
        public NumberExpression<T, U> subtract(TypedExpression<T, U> expression) {
            return new _Number<>(this, operate(Operator.SUBTRACT, expression));
        }

        @Override
        public NumberExpression<T, U> multiply(TypedExpression<T, U> expression) {
            return new _Number<>(this, operate(Operator.MULTIPLY, expression));
        }

        @Override
        public NumberExpression<T, U> divide(TypedExpression<T, U> expression) {
            return new _Number<>(this, operate(Operator.DIVIDE, expression));
        }

        @Override
        public NumberExpression<T, U> mod(TypedExpression<T, U> expression) {
            return new _Number<>(this, operate(Operator.MOD, expression));
        }

        @Override
        public NumberExpression<T, U> sum() {
            return new _Number<>(this, operate(Operator.SUM, Lists.of()));
        }

        @Override
        public NumberExpression<T, Double> avg() {
            return new _Number<>(this, operate(Operator.AVG, Lists.of()));
        }

        @Override
        public NumberExpression<T, U> max() {
            return new _Number<>(this, operate(Operator.MAX, Lists.of()));
        }

        @Override
        public NumberExpression<T, U> min() {
            return new _Number<>(this, operate(Operator.MIN, Lists.of()));
        }

        @Override
        public NumberExpression<T, U> addIfNotNull(U value) {
            return value == null ? this : add(value);
        }

        @Override
        public NumberExpression<T, U> subtractIfNotNull(U value) {
            return value == null ? this : subtract(value);
        }

        @Override
        public NumberExpression<T, U> multiplyIfNotNull(U value) {
            return value == null ? this : multiply(value);
        }

        @Override
        public NumberExpression<T, U> divideIfNotNull(U value) {
            return value == null ? this : divide(value);
        }

        @Override
        public NumberExpression<T, U> modIfNotNull(U value) {
            return value == null ? this : mod(value);
        }
    }

    @Accessors(fluent = true)
    static class _Predicate<T> implements Predicate<T> {
        private final Expression expression;

        private _Predicate(Expression expression) {
            this.expression = expression;
        }

        @Override
        public Predicate<T> not() {
            return new _Predicate<>(Expressions.operate(expression, Operator.NOT));
        }

        @Override
        public Expression expression() {
            return expression;
        }
    }

    static class _String<T>
            extends _Comparable<T, String>
            implements StringPathExpression<T> {

        public _String(_Basic<?, ?> origin, Expression operand) {
            super(origin, operand);
        }

        public _String(Operation operation, Expression operand) {
            super(operation, operand);
        }

        @Override
        public BooleanExpression<T> like(String value) {
            return new _Boolean<>(this, operate(Operator.LIKE, value));
        }

        @Override
        public BooleanExpression<T> notLike(String value) {
            Expression operate = operate(Operator.LIKE, value);
            operate = Expressions.operate(operate, Operator.NOT);
            return new _Boolean<>(this, operate);
        }

        @Override
        public BooleanExpression<T> likeIfNotNull(String value) {
            return value == null ? rollback() : like(value);
        }

        @Override
        public BooleanExpression<T> notLikeIfNotNull(String value) {
            return null;
        }

        @Override
        public StringExpression<T> lower() {
            return new _String<>(this, operate(Operator.LOWER, Lists.of()));
        }

        @Override
        public StringExpression<T> upper() {
            return new _String<>(this, operate(Operator.UPPER, Lists.of()));
        }

        @Override
        public StringExpression<T> substring(int a, int b) {
            List<TypedExpression<T, ?>> expressions =
                    Lists.of(of(a), of(b));
            return new _String<>(this, operate(Operator.SUBSTRING, expressions));
        }

        @Override
        public StringExpression<T> substring(int a) {
            return new _String<>(this, operate(Operator.SUBSTRING, a));
        }

        @Override
        public StringExpression<T> trim() {
            return new _String<>(this, operate(Operator.TRIM, Lists.of()));
        }

        @Override
        public NumberExpression<T, Integer> length() {
            return new _Number<>(this, operate(Operator.LENGTH, Lists.of()));
        }
    }
}
