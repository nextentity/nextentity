package io.github.nextentity.core;

import io.github.nextentity.core.ExpressionTrees.OrderImpl;
import io.github.nextentity.core.api.Expression;
import io.github.nextentity.core.api.Expression.Column;
import io.github.nextentity.core.api.ExpressionOperator.AndOperator;
import io.github.nextentity.core.api.ExpressionOperator.NumberOperator;
import io.github.nextentity.core.api.ExpressionOperator.OrOperator;
import io.github.nextentity.core.api.ExpressionOperator.PathOperator;
import io.github.nextentity.core.api.ExpressionOperator.PredicateOperator;
import io.github.nextentity.core.api.ExpressionOperator.StringOperator;
import io.github.nextentity.core.api.Lists;
import io.github.nextentity.core.api.Operator;
import io.github.nextentity.core.api.Path;
import io.github.nextentity.core.api.Path.BooleanPath;
import io.github.nextentity.core.api.Path.NumberPath;
import io.github.nextentity.core.api.Path.StringPath;
import io.github.nextentity.core.api.Query.PredicateBuilder;
import io.github.nextentity.core.api.Root;
import io.github.nextentity.core.api.SortOrder;
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
import lombok.experimental.Accessors;
import org.jetbrains.annotations.NotNull;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import static io.github.nextentity.core.TypedExpressions.TypeExpressionImpl.EMPTY;
import static io.github.nextentity.core.api.Operator.*;

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
        return toTypedExpression(column);
    }

    public static <T, R> EntityPathExpression<T, R> ofEntity(Column column) {
        return toTypedExpression(column);
    }

    public static <T> StringPathExpression<T> ofString(Column column) {
        return toTypedExpression(column);
    }

    public static <T, U extends Number> NumberPathExpression<T, U> ofNumber(Column column) {
        return toTypedExpression(column);
    }

    public static <T, R> BasicExpression<T, R> ofBasic(Expression expression) {
        return toTypedExpression(expression);
    }

    public static <T> StringExpression<T> ofString(Expression expression) {
        return toTypedExpression(expression);
    }

    public static <T> Predicate<T> ofBoolean(Expression expression) {
        return toTypedExpression(expression);
    }

    public static <T> BooleanPathExpression<T> ofBoolean(Column expression) {
        return toTypedExpression(expression);
    }

    public static <T, U extends Number> NumberExpression<T, U> ofNumber(Expression expression) {
        return toTypedExpression(expression);
    }

    public static <T> PredicateOperator<T> ofPredicate(Expression expression) {
        return toTypedExpression(expression);
    }

    static <T extends TypedExpression<?, ?>> T toTypedExpression(Expression expression) {
        AbstractTypeExpression result;
        if (expression == null || expression instanceof AbstractTypeExpression) {
            result = (AbstractTypeExpression) expression;
        } else {
            result = new TypeExpressionImpl(expression.tree());
        }
        return TypeCastUtil.unsafeCast(result);
    }

    static final class TypeExpressionImpl implements AbstractTypeExpression {

        static final TypeExpressionImpl EMPTY = new TypeExpressionImpl(null);

        private final ExpressionTree expression;

        TypeExpressionImpl(ExpressionTree expression) {
            this.expression = expression;
        }

        @Override
        public ExpressionTree tree() {
            return expression;
        }
    }


    @Accessors(fluent = true)
    @SuppressWarnings("rawtypes")
    interface AbstractTypeExpression extends NumberPathExpression, StringPathExpression, BooleanPathExpression, EntityPathExpression {

        @Override
        default Root root() {
            return Paths.root();
        }

        @Override
        default NumberExpression count() {
            return toTypedExpression(Expressions.operate(tree(), COUNT));
        }

        @Override
        default Predicate eq(Object value) {
            return eq(TypedExpressions.of(value));
        }

        @Override
        default Predicate eqIfNotNull(Object value) {
            return value == null ? operateNull() : eq(value);
        }

        @Override
        default Predicate eq(TypedExpression value) {
            return operate(EQ, value);
        }

        @Override
        default Predicate ne(Object value) {
            return ne(TypedExpressions.of(value));
        }

        @Override
        default Predicate neIfNotNull(Object value) {
            return value == null ? operateNull() : ne(value);
        }

        @Override
        default Predicate ne(TypedExpression value) {
            return operate(NE, value);
        }

        @Override
        default Predicate in(@NotNull TypedExpression expressions) {
            return operate(IN, expressions);
        }

        @Override
        default Predicate in(Object[] values) {
            List<TypedExpression<?, ?>> collect = Arrays.stream(values)
                    .map(TypedExpressions::of)
                    .collect(Collectors.toList());
            return in(collect);
        }

        @Override
        default Predicate in(@NotNull Collection values) {
            List<TypedExpression<?, ?>> collect = ((Collection<?>) values).stream()
                    .map(TypedExpressions::of)
                    .collect(Collectors.toList());
            return in(collect);
        }

        @Override
        default Predicate notIn(Object[] values) {
            return not(in(values));
        }

        @Override
        default Predicate notIn(@NotNull Collection values) {
            return not(in(values));
        }

        @Override
        default Predicate isNull() {
            return operate(IS_NULL);
        }

        @Override
        default Predicate isNotNull() {
            return not(isNull());
        }

        @Override
        default Predicate notIn(@NotNull List values) {
            return not(in(values));
        }

        @Override
        default Predicate in(@NotNull List expressions) {
            return operate(IN, asTypeExpressions(expressions));
        }

        @Override
        default Predicate ge(TypedExpression expression) {
            return operate(GE, expression);
        }

        @Override
        default Predicate gt(TypedExpression expression) {
            return operate(GT, expression);
        }

        @Override
        default Predicate le(TypedExpression expression) {
            return operate(LE, expression);
        }

        @Override
        default Predicate lt(TypedExpression expression) {
            return operate(LT, expression);
        }

        @Override
        default Predicate between(TypedExpression l, TypedExpression r) {
            return operate(BETWEEN, List.of(l, r));
        }

        @Override
        default Predicate notBetween(TypedExpression l, TypedExpression r) {
            return not(between(l, r));
        }

        @Override
        default Order sort(SortOrder order) {
            return new OrderImpl(tree(), order);
        }

        @Override
        default Predicate geIfNotNull(Object value) {
            return value == null ? operateNull() : ge(TypedExpressions.of(value));
        }

        @Override
        default Predicate gtIfNotNull(Object value) {
            return value == null ? operateNull() : gt(TypedExpressions.of(value));
        }

        @Override
        default Predicate leIfNotNull(Object value) {
            return value == null ? operateNull() : le(TypedExpressions.of(value));
        }

        @Override
        default Predicate ltIfNotNull(Object value) {
            return value == null ? operateNull() : lt(TypedExpressions.of(value));
        }

        @Override
        default NumberExpression add(TypedExpression expression) {
            return operate(ADD, expression);
        }

        @Override
        default NumberExpression subtract(TypedExpression expression) {
            return operate(SUBTRACT, expression);
        }

        @Override
        default NumberExpression multiply(TypedExpression expression) {
            return operate(MULTIPLY, expression);
        }

        @Override
        default NumberExpression divide(TypedExpression expression) {
            return operate(DIVIDE, expression);
        }

        @Override
        default NumberExpression mod(TypedExpression expression) {
            return operate(MOD, expression);
        }

        @Override
        default NumberExpression sum() {
            return operate(SUM);
        }

        @Override
        default NumberExpression avg() {
            return operate(AVG);
        }

        @Override
        default NumberExpression max() {
            return operate(MAX);
        }

        @Override
        default NumberExpression min() {
            return operate(MIN);
        }

        @Override
        default Predicate like(String value) {
            return operate(LIKE, Expressions.of(value));
        }

        @Override
        default Predicate notLike(String value) {
            return not(like(value));
        }

        @Override
        default Predicate likeIfNotNull(String value) {
            return value == null ? operateNull() : like(value);
        }

        @Override
        default Predicate notLikeIfNotNull(String value) {
            return value == null ? operateNull() : notLike(value);
        }

        @Override
        default StringExpression lower() {
            return operate(LOWER);
        }

        @Override
        default StringExpression upper() {
            return operate(UPPER);
        }

        @Override
        default StringExpression substring(int a, int b) {
            return operate0(SUBSTRING, Lists.of(Expressions.of(a), Expressions.of(b)));
        }

        @Override
        default StringExpression substring(int a) {
            return operate(SUBSTRING, Expressions.of(a));
        }

        @Override
        default StringExpression trim() {
            return operate(TRIM);
        }

        @Override
        default NumberExpression length() {
            return operate(LENGTH);
        }

        @Override
        default PathOperator and(Path path) {
            return ExpressionOperators.ofPath(of(path).asBasic(), this::and);
        }

        @NotNull
        default AbstractTypeExpression and(BasicExpression<?, ?> basicExpression) {
            return basicExpression == null ? this : operate(AND, basicExpression);
        }

        @NotNull
        default AbstractTypeExpression or(BasicExpression<?, ?> basicExpression) {
            return basicExpression == null ? this : operate(OR, basicExpression);
        }

        @NotNull
        static AbstractTypeExpression of(Path path) {
            return toTypedExpression(Expressions.of(path));
        }

        @Override
        default NumberOperator and(NumberPath path) {
            return ExpressionOperators.ofNumber(of(path).asNumber(), this::and);
        }

        @Override
        default StringOperator and(StringPath path) {
            return ExpressionOperators.ofString(of(path).asString(), this::and);
        }

        @Override
        default AndOperator andIf(boolean predicate, PredicateBuilder predicateBuilder) {
            return predicate ? and(((PredicateBuilder<?>) predicateBuilder).build(TypeCastUtil.cast(root()))) : this;
        }

        @Override
        default PathOperator or(Path path) {
            return ExpressionOperators.ofPath(of(path).asBasic(), this::or);
        }

        @Override
        default NumberOperator or(NumberPath path) {
            return ExpressionOperators.ofNumber(of(path).asNumber(), this::or);
        }


        @Override
        default StringOperator or(StringPath path) {
            return ExpressionOperators.ofString(of(path).asString(), this::or);
        }

        @Override
        default OrOperator orIf(boolean predicate, PredicateBuilder predicateBuilder) {
            return predicate ? or(((PredicateBuilder<?>) predicateBuilder).build(TypeCastUtil.cast(root()))) : this;
        }

        @Override
        default OrOperator or(List list) {
            return operate(OR, asTypeExpressions(list));
        }

        @Override
        default OrOperator or(TypedExpression predicate) {
            return operate(OR, predicate);
        }

        @Override
        default AndOperator and(List list) {
            return operate(AND, asTypeExpressions(list));
        }

        @Override
        default AndOperator and(TypedExpression expression) {
            return operate(AND, expression);
        }

        @Override
        default EntityPathExpression get(Path path) {
            Column expression = (Column) Paths.get((Path<?, ?>) path).tree();
            return toTypedExpression(((Column) tree()).get(expression));
        }

        @Override
        default StringPathExpression get(StringPath path) {
            return get0(path);
        }

        @Override
        default StringPathExpression get(StringPathExpression path) {
            return get0(path);
        }

        @Override
        default BooleanPathExpression get(BooleanPath path) {
            return get0(path);
        }

        @Override
        default NumberPathExpression get(NumberPathExpression path) {
            return get0(path);
        }

        @Override
        default PathExpression get(PathExpression path) {
            return get0(path);
        }

        @Override
        default NumberPathExpression get(NumberPath path) {
            return get0(path);
        }

        @Override
        default Predicate not() {
            return operate(NOT);
        }

        default AbstractTypeExpression get0(Path<?, ?> path) {
            PathExpression<?, ?> pathExpression = Paths.get((Path<?, ?>) path);
            return get0(pathExpression);
        }

        @NotNull
        private AbstractTypeExpression get0(PathExpression<?, ?> pathExpression) {
            Column expression = (Column) pathExpression.tree();
            ExpressionTree expr = tree();
            return toTypedExpression(((Column) expr).get(expression));
        }

        default AbstractTypeExpression not(TypedExpression<?, ?> expression) {
            ExpressionTree operate = Expressions.operate(expression.tree(), NOT);
            return toTypedExpression(operate);
        }

        @NotNull
        default AbstractTypeExpression operate(Operator operator, Expression expression) {
            return toTypedExpression(Expressions.operate(tree(), operator, expression.tree()));
        }

        @NotNull
        default AbstractTypeExpression operate(Operator operator, List<? extends Expression> expressions) {
            List<Expression> list = expressions.stream().map(Expression::tree).collect(Collectors.toList());
            return operate0(operator, list);
        }

        @NotNull
        default AbstractTypeExpression operate0(Operator operator, List<Expression> list) {
            return toTypedExpression(Expressions.operate(tree(), operator, list));
        }

        @NotNull
        default AbstractTypeExpression operate(Operator operator) {
            return toTypedExpression(Expressions.operate(tree(), operator));
        }

        @NotNull
        static TypedExpression.Predicate operateNull() {
            return EMPTY;
        }

        @NotNull
        default StringExpression<?> asString() {
            return this;
        }

        default BasicExpression<?, ?> asBasic() {
            return this;
        }

        default NumberExpression<?, ?> asNumber() {
            return this;
        }

        default List<? extends TypedExpression<?, ?>> asTypeExpressions(List<?> list) {
            return TypeCastUtil.cast(list);
        }

    }
}
