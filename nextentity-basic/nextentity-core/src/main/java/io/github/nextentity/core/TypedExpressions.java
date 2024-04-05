package io.github.nextentity.core;

import io.github.nextentity.core.ExpressionTrees.OrderImpl;
import io.github.nextentity.core.api.Expression.Column;
import io.github.nextentity.core.api.Expression;
import io.github.nextentity.core.api.ExpressionOperator.AndOperator;
import io.github.nextentity.core.api.ExpressionOperator.NumberOperator;
import io.github.nextentity.core.api.ExpressionOperator.OrOperator;
import io.github.nextentity.core.api.ExpressionOperator.PathOperator;
import io.github.nextentity.core.api.ExpressionOperator.PredicateOperator;
import io.github.nextentity.core.api.ExpressionOperator.StringOperator;
import io.github.nextentity.core.api.Lists;
import io.github.nextentity.core.api.Operator;
import io.github.nextentity.core.api.Order;
import io.github.nextentity.core.api.Order.SortOrder;
import io.github.nextentity.core.api.Path;
import io.github.nextentity.core.api.Path.BooleanPath;
import io.github.nextentity.core.api.Path.NumberPath;
import io.github.nextentity.core.api.Path.StringPath;
import io.github.nextentity.core.api.Query.PredicateBuilder;
import io.github.nextentity.core.api.Root;
import io.github.nextentity.core.api.TypedExpression;
import io.github.nextentity.core.api.TypedExpression.BasicExpression;
import io.github.nextentity.core.api.TypedExpression.BooleanPathExpression;
import io.github.nextentity.core.api.TypedExpression.EntityPathExpression;
import io.github.nextentity.core.api.TypedExpression.NumberExpression;
import io.github.nextentity.core.api.TypedExpression.NumberPathExpression;
import io.github.nextentity.core.api.TypedExpression.PathExpression;
import io.github.nextentity.core.api.TypedExpression.Predicate;
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

import static io.github.nextentity.core.api.Operator.ADD;
import static io.github.nextentity.core.api.Operator.AND;
import static io.github.nextentity.core.api.Operator.AVG;
import static io.github.nextentity.core.api.Operator.BETWEEN;
import static io.github.nextentity.core.api.Operator.COUNT;
import static io.github.nextentity.core.api.Operator.DIVIDE;
import static io.github.nextentity.core.api.Operator.EQ;
import static io.github.nextentity.core.api.Operator.GE;
import static io.github.nextentity.core.api.Operator.GT;
import static io.github.nextentity.core.api.Operator.IN;
import static io.github.nextentity.core.api.Operator.IS_NULL;
import static io.github.nextentity.core.api.Operator.LE;
import static io.github.nextentity.core.api.Operator.LENGTH;
import static io.github.nextentity.core.api.Operator.LIKE;
import static io.github.nextentity.core.api.Operator.LOWER;
import static io.github.nextentity.core.api.Operator.LT;
import static io.github.nextentity.core.api.Operator.MAX;
import static io.github.nextentity.core.api.Operator.MIN;
import static io.github.nextentity.core.api.Operator.MOD;
import static io.github.nextentity.core.api.Operator.MULTIPLY;
import static io.github.nextentity.core.api.Operator.NE;
import static io.github.nextentity.core.api.Operator.NOT;
import static io.github.nextentity.core.api.Operator.OR;
import static io.github.nextentity.core.api.Operator.SUBSTRING;
import static io.github.nextentity.core.api.Operator.SUBTRACT;
import static io.github.nextentity.core.api.Operator.SUM;
import static io.github.nextentity.core.api.Operator.TRIM;
import static io.github.nextentity.core.api.Operator.UPPER;

public class TypedExpressions {

    public static <T, U> TypedExpression<T, U> of(Expression expression) {
        return expression instanceof TypedExpression<?, ?>
                ? TypeCastUtil.cast(expression)
                : expression::tree;
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
        return newTypedExpression(column);
    }

    public static <T, R> EntityPathExpression<T, R> ofEntity(Column column) {
        return newTypedExpression(column);
    }

    public static <T> StringPathExpression<T> ofString(Column column) {
        return newTypedExpression(column);
    }

    public static <T, U extends Number> NumberPathExpression<T, U> ofNumber(Column column) {
        return newTypedExpression(column);
    }

    public static <T, R> BasicExpression<T, R> ofBasic(Expression expression) {
        return newTypedExpression(expression);
    }

    public static <T> StringExpression<T> ofString(Expression expression) {
        return newTypedExpression(expression);
    }

    public static <T> Predicate<T> ofBoolean(Expression expression) {
        return newTypedExpression(expression);
    }

    public static <T> BooleanPathExpression<T> ofBoolean(Column expression) {
        return newTypedExpression(expression);
    }

    public static <T, U extends Number> NumberExpression<T, U> ofNumber(Expression expression) {
        return newTypedExpression(expression);
    }

    public static <T> PredicateOperator<T> ofPredicate(Expression expression) {
        return newTypedExpression(expression);
    }

    private static <T extends TypedExpression<?, ?>> T newTypedExpression(Expression expression) {
        return TypeCastUtil.unsafeCast(new RawTypeExpression(expression));
    }

    static RawTypeExpression raw(TypedExpression<?, ?> expression) {
        RawTypeExpression result;
        if (expression == null || expression instanceof RawTypeExpression) {
            result = (RawTypeExpression) expression;
        } else {
            result = new RawTypeExpression(expression.tree());
        }
        return result;
    }


    @Data
    @Accessors(fluent = true)
    @SuppressWarnings("rawtypes")
    static final class RawTypeExpression implements NumberPathExpression, StringPathExpression, BooleanPathExpression, EntityPathExpression {

        static final RawTypeExpression EMPTY = new RawTypeExpression(null);

        final Expression expression;

        public RawTypeExpression(Expression expression) {
            this.expression = expression;
        }

        @Override
        public Root root() {
            return Paths.root();
        }

        @Override
        public NumberExpression count() {
            return new RawTypeExpression(Expressions.operate(tree(), COUNT));
        }

        @Override
        public Predicate eq(Object value) {
            return eq(TypedExpressions.of(value));
        }

        @Override
        public Predicate eqIfNotNull(Object value) {
            return value == null ? operateNull() : eq(value);
        }

        @Override
        public Predicate eq(TypedExpression value) {
            return operate(EQ, value);
        }

        @Override
        public Predicate ne(Object value) {
            return ne(TypedExpressions.of(value));
        }

        @Override
        public Predicate neIfNotNull(Object value) {
            return value == null ? operateNull() : ne(value);
        }

        @Override
        public Predicate ne(TypedExpression value) {
            return operate(NE, value);
        }

        @Override
        public Predicate in(@NotNull TypedExpression expressions) {
            return operate(IN, expressions);
        }

        @Override
        public Predicate in(Object[] values) {
            List<TypedExpression<?, ?>> collect = Arrays.stream(values)
                    .map(TypedExpressions::of)
                    .collect(Collectors.toList());
            return in(collect);
        }

        @Override
        public Predicate in(@NotNull Collection values) {
            List<TypedExpression<?, ?>> collect = ((Collection<?>) values).stream()
                    .map(TypedExpressions::of)
                    .collect(Collectors.toList());
            return in(collect);
        }

        @Override
        public Predicate notIn(Object[] values) {
            return not(in(values));
        }

        @Override
        public Predicate notIn(@NotNull Collection values) {
            return not(in(values));
        }

        @Override
        public Predicate isNull() {
            return operate(IS_NULL);
        }

        @Override
        public Predicate isNotNull() {
            return not(isNull());
        }

        @Override
        public Predicate notIn(@NotNull List values) {
            return not(in(values));
        }

        @Override
        public Predicate in(@NotNull List expressions) {
            return operate(IN, asTypeExpressions(expressions));
        }

        @Override
        public Predicate ge(TypedExpression expression) {
            return operate(GE, expression);
        }

        @Override
        public Predicate gt(TypedExpression expression) {
            return operate(GT, expression);
        }

        @Override
        public Predicate le(TypedExpression expression) {
            return operate(LE, expression);
        }

        @Override
        public Predicate lt(TypedExpression expression) {
            return operate(LT, expression);
        }

        @Override
        public Predicate between(TypedExpression l, TypedExpression r) {
            return operate(BETWEEN, List.of(l, r));
        }

        @Override
        public Predicate notBetween(TypedExpression l, TypedExpression r) {
            return not(between(l, r));
        }

        @Override
        public Order sort(SortOrder order) {
            return new OrderImpl(tree(), order);
        }

        @Override
        public Predicate geIfNotNull(Object value) {
            return value == null ? operateNull() : ge(TypedExpressions.of(value));
        }

        @Override
        public Predicate gtIfNotNull(Object value) {
            return value == null ? operateNull() : gt(TypedExpressions.of(value));
        }

        @Override
        public Predicate leIfNotNull(Object value) {
            return value == null ? operateNull() : le(TypedExpressions.of(value));
        }

        @Override
        public Predicate ltIfNotNull(Object value) {
            return value == null ? operateNull() : lt(TypedExpressions.of(value));
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
        public Predicate like(String value) {
            return operate(LIKE, Expressions.of(value));
        }

        @Override
        public Predicate notLike(String value) {
            return not(like(value));
        }

        @Override
        public Predicate likeIfNotNull(String value) {
            return value == null ? operateNull() : like(value);
        }

        @Override
        public Predicate notLikeIfNotNull(String value) {
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
        RawTypeExpression and(BasicExpression<?, ?> basicExpression) {
            return basicExpression == null ? this : operate(AND, basicExpression);
        }

        @NotNull
        RawTypeExpression or(BasicExpression<?, ?> basicExpression) {
            return basicExpression == null ? this : operate(OR, basicExpression);
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
        public StringOperator and(StringPath path) {
            return ExpressionOperators.ofString(of(path).asString(), this::and);
        }

        @Override
        public AndOperator andIf(boolean predicate, PredicateBuilder predicateBuilder) {
            return predicate ? and(((PredicateBuilder<?>) predicateBuilder).build(TypeCastUtil.cast(root()))) : this;
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
        public StringOperator or(StringPath path) {
            return ExpressionOperators.ofString(of(path).asString(), this::or);
        }

        @Override
        public OrOperator orIf(boolean predicate, PredicateBuilder predicateBuilder) {
            return predicate ? or(((PredicateBuilder<?>) predicateBuilder).build(TypeCastUtil.cast(root()))) : this;
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
        public AndOperator and(List list) {
            return operate(AND, asTypeExpressions(list));
        }

        @Override
        public AndOperator and(TypedExpression expression) {
            return operate(AND, expression);
        }

        @Override
        public EntityPathExpression get(Path path) {
            Column expression = (Column) Paths.get((Path<?, ?>) path).tree();
            return new RawTypeExpression(((Column) tree()).get(expression));
        }

        @Override
        public StringPathExpression get(StringPath path) {
            return get0(path);
        }

        @Override
        public StringPathExpression get(StringPathExpression path) {
            return get0(path);
        }

        @Override
        public BooleanPathExpression get(BooleanPath path) {
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
        public NumberPathExpression get(NumberPath path) {
            return get0(path);
        }

        @Override
        public Predicate not() {
            return operate(NOT);
        }

        RawTypeExpression get0(Path<?, ?> path) {
            PathExpression<?, ?> pathExpression = Paths.get((Path<?, ?>) path);
            return get0(pathExpression);
        }

        @NotNull
        private RawTypeExpression get0(PathExpression<?, ?> pathExpression) {
            Column expression = (Column) pathExpression.tree();
            Expression expr = tree();
            return new RawTypeExpression(((Column) expr).get(expression));
        }

        RawTypeExpression not(TypedExpression<?, ?> expression) {
            Expression operate = Expressions.operate(expression.tree(), NOT);
            return new RawTypeExpression(operate);
        }

        @NotNull
        RawTypeExpression operate(Operator operator, Expression expression) {
            return new RawTypeExpression(Expressions.operate(tree(), operator, expression.tree()));
        }

        @NotNull
        RawTypeExpression operate(Operator operator, List<? extends Expression> expressions) {
            List<Expression> list = expressions.stream().map(Expression::tree).collect(Collectors.toList());
            return operate0(operator, list);
        }

        @NotNull
        RawTypeExpression operate0(Operator operator, List<Expression> list) {
            return new RawTypeExpression(Expressions.operate(tree(), operator, list));
        }

        @NotNull
        RawTypeExpression operate(Operator operator) {
            return new RawTypeExpression(Expressions.operate(tree(), operator));
        }

        @NotNull
        static TypedExpression.Predicate operateNull() {
            return EMPTY;
        }

        @NotNull
        StringExpression<?> asString() {
            return this;
        }

        BasicExpression<?, ?> asBasic() {
            return this;
        }

        NumberExpression<?, ?> asNumber() {
            return this;
        }

        List<? extends TypedExpression<?, ?>> asTypeExpressions(List<?> list) {
            return TypeCastUtil.cast(list);
        }

        @Override
        public ExpressionTree tree() {
            return expression == null ? null : expression.tree();
        }
    }
}
