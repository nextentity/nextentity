package io.github.nextentity.core;

import io.github.nextentity.core.QueryStructures.OrderImpl;
import io.github.nextentity.core.api.Column;
import io.github.nextentity.core.api.Expression;
import io.github.nextentity.core.api.ExpressionOperator.AndOperator;
import io.github.nextentity.core.api.ExpressionOperator.ComparableOperator;
import io.github.nextentity.core.api.ExpressionOperator.NumberOperator;
import io.github.nextentity.core.api.ExpressionOperator.OrOperator;
import io.github.nextentity.core.api.ExpressionOperator.PathOperator;
import io.github.nextentity.core.api.ExpressionOperator.StringOperator;
import io.github.nextentity.core.api.Lists;
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
import io.github.nextentity.core.api.TypedExpression.StringExpression;
import io.github.nextentity.core.api.TypedExpression.StringPathExpression;
import io.github.nextentity.core.util.Paths;
import lombok.Data;
import lombok.experimental.Accessors;
import org.jetbrains.annotations.NotNull;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import static io.github.nextentity.core.api.Operator.*;

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
        return TypeCastUtil.unsafeCast(new RawTypeExpression(column));
    }

    public static <T, R> EntityPathExpression<T, R> ofEntity(Column column) {
        return TypeCastUtil.unsafeCast(new RawTypeExpression(column));
    }

    public static <T> StringPathExpression<T> ofString(Column column) {
        return TypeCastUtil.unsafeCast(new RawTypeExpression(column));
    }

    public static <T> BooleanPathExpression<T> ofBoolean(Column column) {
        return TypeCastUtil.unsafeCast(new RawTypeExpression(column));
    }

    public static <T, U extends Number & Comparable<U>> NumberPathExpression<T, U> ofNumber(Column column) {
        return TypeCastUtil.unsafeCast(new RawTypeExpression(column));
    }

    public static <T, U extends Comparable<U>> ComparablePathExpression<T, U> ofComparable(Column column) {
        return TypeCastUtil.unsafeCast(new RawTypeExpression(column));
    }

    public static <T, R> BasicExpression<T, R> ofBasic(Expression expression) {
        return TypeCastUtil.unsafeCast(new RawTypeExpression(expression));
    }

    public static <T> StringExpression<T> ofString(Expression expression) {
        return TypeCastUtil.unsafeCast(new RawTypeExpression(expression));
    }

    public static <T> BooleanExpression<T> ofBoolean(Expression expression) {
        return TypeCastUtil.unsafeCast(new RawTypeExpression(expression));
    }

    public static <T, U extends Number & Comparable<U>> NumberExpression<T, U> ofNumber(Expression expression) {
        return TypeCastUtil.unsafeCast(new RawTypeExpression(expression));
    }

    public static <T, U extends Comparable<U>> ComparableExpression<T, U> ofComparable(Expression expression) {
        return TypeCastUtil.unsafeCast(new RawTypeExpression(expression));
    }


    @Data
    @Accessors(fluent = true)
    @SuppressWarnings("rawtypes")
    private static class RawTypeExpression implements ComparablePathExpression, NumberPathExpression, StringPathExpression, BooleanPathExpression, EntityPathExpression {

        protected static final RawTypeExpression EMPTY = new RawTypeExpression(null);

        protected final Expression expression;

        public RawTypeExpression(Expression expression) {
            this.expression = expression;
        }

        @Override
        public Root root() {
            return Paths.root();
        }

        @Override
        public NumberExpression count() {
            return new RawTypeExpression(Expressions.operate(expression(), COUNT));
        }

        @Override
        public BooleanExpression eq(Object value) {
            return eq(Expressions.of(value));
        }

        @Override
        public BooleanExpression eqIfNotNull(Object value) {
            return value == null ? operateNull() : eq(value);
        }

        @Override
        public BooleanExpression eq(TypedExpression value) {
            return operate(EQ, value);
        }

        @Override
        public BooleanExpression ne(Object value) {
            return ne(Expressions.of(value));
        }

        @Override
        public BooleanExpression neIfNotNull(Object value) {
            return value == null ? operateNull() : ne(value);
        }

        @Override
        public BooleanExpression ne(TypedExpression value) {
            return operate(NE, value);
        }

        @Override
        public BooleanExpression in(Object[] values) {
            List<TypedExpression<?, ?>> collect = Arrays.stream(values)
                    .map(TypedExpressions::of)
                    .collect(Collectors.toList());
            return in(collect);
        }

        @Override
        public BooleanExpression in(@NotNull Collection values) {
            List<TypedExpression<?, ?>> collect = ((Collection<?>) values).stream()
                    .map(TypedExpressions::of)
                    .collect(Collectors.toList());
            return in(collect);
        }

        @Override
        public BooleanExpression notIn(Object[] values) {
            return not(in(values));
        }

        @Override
        public BooleanExpression notIn(@NotNull Collection values) {
            return not(in(values));
        }

        @Override
        public BooleanExpression isNull() {
            return operate(IS_NULL);
        }

        @Override
        public BooleanExpression isNotNull() {
            return not(isNull());
        }

        @Override
        public BooleanExpression notIn(@NotNull List values) {
            return not(in(values));
        }

        @Override
        public BooleanExpression in(@NotNull List expressions) {
            return operate(IN, asTypeExpressions(expressions));
        }

        @Override
        public BooleanExpression ge(TypedExpression expression) {
            return operate(GE, expression);
        }

        @Override
        public BooleanExpression gt(TypedExpression expression) {
            return operate(GT, expression);
        }

        @Override
        public BooleanExpression le(TypedExpression expression) {
            return operate(LE, expression);
        }

        @Override
        public BooleanExpression lt(TypedExpression expression) {
            return operate(LT, expression);
        }

        @Override
        public BooleanExpression between(TypedExpression l, TypedExpression r) {
            return operate(BETWEEN, List.of(l, r));
        }

        @Override
        public BooleanExpression notBetween(TypedExpression l, TypedExpression r) {
            return not(between(l, r));
        }

        @Override
        public Order sort(SortOrder order) {
            return new OrderImpl(expression(), order);
        }

        @Override
        public BooleanExpression geIfNotNull(Comparable value) {
            return value == null ? operateNull() : ge(Expressions.of(value));
        }

        @Override
        public BooleanExpression gtIfNotNull(Comparable value) {
            return value == null ? operateNull() : gt(Expressions.of(value));
        }

        @Override
        public BooleanExpression leIfNotNull(Comparable value) {
            return value == null ? operateNull() : le(Expressions.of(value));
        }

        @Override
        public BooleanExpression ltIfNotNull(Comparable value) {
            return value == null ? operateNull() : lt(Expressions.of(value));
        }

        @Override
        public NumberExpression add(TypedExpression expression) {
            return operate(ADD, expression);
        }

        @Override
        public NumberExpression subtract(TypedExpression expression) {
            return operate(SUBTRACT, expression);
        }

        @Override
        public NumberExpression multiply(TypedExpression expression) {
            return operate(MULTIPLY, expression);
        }

        @Override
        public NumberExpression divide(TypedExpression expression) {
            return operate(DIVIDE, expression);
        }

        @Override
        public NumberExpression mod(TypedExpression expression) {
            return operate(MOD, expression);
        }

        @Override
        public NumberExpression sum() {
            return operate(SUM);
        }

        @Override
        public NumberExpression avg() {
            return operate(AVG);
        }

        @Override
        public NumberExpression max() {
            return operate(MAX);
        }

        @Override
        public NumberExpression min() {
            return operate(MIN);
        }

        @Override
        public NumberExpression addIfNotNull(Number value) {
            return operate(MAX);
        }

        @Override
        public NumberExpression subtractIfNotNull(Number value) {
            return value == null ? this : subtract(Expressions.of(value));
        }

        @Override
        public NumberExpression multiplyIfNotNull(Number value) {
            return value == null ? this : multiply(Expressions.of(value));
        }

        @Override
        public NumberExpression divideIfNotNull(Number value) {
            return value == null ? this : divide(Expressions.of(value));
        }

        @Override
        public NumberExpression modIfNotNull(Number value) {
            return value == null ? this : mod(Expressions.of(value));
        }

        @Override
        public BooleanExpression like(String value) {
            return operate(LIKE, Expressions.of(value));
        }

        @Override
        public BooleanExpression notLike(String value) {
            return not(like(value));
        }

        @Override
        public BooleanExpression likeIfNotNull(String value) {
            return value == null ? operateNull() : like(value);
        }

        @Override
        public BooleanExpression notLikeIfNotNull(String value) {
            return value == null ? operateNull() : notLike(value);
        }

        @Override
        public StringExpression lower() {
            return operate(LOWER);
        }

        @Override
        public StringExpression upper() {
            return operate(UPPER);
        }

        @Override
        public StringExpression substring(int a, int b) {
            return operate0(SUBSTRING, Lists.of(Expressions.of(a), Expressions.of(b)));
        }

        @Override
        public StringExpression substring(int a) {
            return operate(SUBSTRING, Expressions.of(a));
        }

        @Override
        public StringExpression trim() {
            return operate(TRIM);
        }

        @Override
        public NumberExpression length() {
            return operate(LENGTH);
        }

        @Override
        public PathOperator and(Path path) {
            return ExpressionOperators.ofPath(of(path).asBasic(), this::and);
        }

        @NotNull
        protected RawTypeExpression and(BasicExpression<?, ?> basicExpression) {
            return basicExpression == null ? this : operate(AND, basicExpression);
        }

        @NotNull
        protected RawTypeExpression or(BasicExpression<?, ?> basicExpression) {
            return basicExpression == null ? this : operate(OR, basicExpression);
        }

        @Override
        public ComparableOperator and(ComparablePath path) {
            return ExpressionOperators.ofComparable(of(path).asComparable(), this::and);
        }

        @NotNull
        static RawTypeExpression of(Path path) {
            return new RawTypeExpression(Expressions.of(path));
        }

        @Override
        public NumberOperator and(NumberPath path) {
            return ExpressionOperators.ofNumber(of(path).asNumber(), this::and);
        }

        @Override
        public ComparableOperator and(BooleanPath path) {
            return ExpressionOperators.ofComparable(of(path).asComparable(), this::and);
        }

        @Override
        public StringOperator and(StringPath path) {
            return ExpressionOperators.ofString(of(path).asString(), this::and);
        }

        @Override
        public PathOperator or(Path path) {
            return ExpressionOperators.ofPath(of(path).asBasic(), this::or);
        }

        @Override
        public NumberOperator or(NumberPath path) {
            return ExpressionOperators.ofNumber(of(path).asNumber(), this::or);
        }

        @Override
        public ComparableOperator or(ComparablePath path) {
            return ExpressionOperators.ofComparable(of(path).asComparable(), this::or);
        }

        @Override
        public StringOperator or(StringPath path) {
            return ExpressionOperators.ofString(of(path).asString(), this::or);
        }

        @Override
        public ComparableOperator or(BooleanPath path) {
            return ExpressionOperators.ofComparable(of(path).asComparable(), this::or);
        }

        @Override
        public OrOperator or(List list) {
            return operate(OR, asTypeExpressions(list));
        }

        @Override
        public OrOperator or(TypedExpression predicate) {
            return operate(OR, predicate);
        }

        @Override
        public Predicate then() {
            return this;
        }

        @Override
        public AndOperator and(List list) {
            return operate(AND, asTypeExpressions(list));
        }

        @Override
        public AndOperator and(TypedExpression expression) {
            return operate(AND, expression);
        }

        @Override
        public Predicate not() {
            return operate(NOT);
        }

        @Override
        public EntityPathExpression get(Path path) {
            Column expression = (Column) Paths.get((Path<?, ?>) path).expression();
            return new RawTypeExpression(((Column) expression()).get(expression));
        }

        @Override
        public StringPathExpression get(StringPath path) {
            return get0(path);
        }

        @Override
        public BooleanPathExpression get(BooleanPath path) {
            return get0(path);
        }

        @Override
        public StringPathExpression get(StringPathExpression path) {
            return get0(path);
        }

        @Override
        public BooleanPathExpression get(BooleanPathExpression path) {
            return get0(path);
        }

        @Override
        public ComparablePathExpression get(ComparablePathExpression path) {
            return get0(path);
        }

        @Override
        public NumberPathExpression get(NumberPathExpression path) {
            return get0(path);
        }

        @Override
        public PathExpression get(PathExpression path) {
            return get0(path);
        }

        @Override
        public ComparablePathExpression get(ComparablePath path) {
            return get0(path);
        }

        @Override
        public NumberPathExpression get(NumberPath path) {
            return get0(path);
        }

        protected RawTypeExpression get0(Path<?, ?> path) {
            PathExpression<?, ?> pathExpression = Paths.get((Path<?, ?>) path);
            return get0(pathExpression);
        }

        @NotNull
        private RawTypeExpression get0(PathExpression<?, ?> pathExpression) {
            Column expression = (Column) pathExpression.expression();
            Expression expr = expression();
            return new RawTypeExpression(((Column) expr).get(expression));
        }

        RawTypeExpression not(TypedExpression<?, ?> expression) {
            Expression operate = Expressions.operate(expression.expression(), NOT);
            return new RawTypeExpression(operate);
        }

        @NotNull
        protected RawTypeExpression operate(Operator operator, TypedExpression expression) {
            return new RawTypeExpression(Expressions.operate(expression(), operator, expression.expression()));
        }

        @NotNull
        protected RawTypeExpression operate(Operator operator, List<? extends TypedExpression> expressions) {
            List<Expression> list = expressions.stream().map(TypedExpression::expression).collect(Collectors.toList());
            return operate0(operator, list);
        }

        @NotNull
        protected RawTypeExpression operate0(Operator operator, List<Expression> list) {
            return new RawTypeExpression(Expressions.operate(expression(), operator, list));
        }

        @NotNull
        protected RawTypeExpression operate(Operator operator) {
            return new RawTypeExpression(Expressions.operate(expression(), operator));
        }

        @NotNull
        protected static BooleanExpression operateNull() {
            return EMPTY;
        }

        @NotNull
        protected StringExpression<?> asString() {
            return this;
        }

        protected ComparableExpression<?, ?> asComparable() {
            return this;
        }

        protected BasicExpression<?, ?> asBasic() {
            return this;
        }

        protected NumberExpression<?, ?> asNumber() {
            return this;
        }

        List<? extends TypedExpression<?, ?>> asTypeExpressions(List<?> list) {
            return TypeCastUtil.cast(list);
        }

    }
}
