package io.github.nextentity.core;

import io.github.nextentity.core.ExpressionTrees.OrderImpl;
import io.github.nextentity.core.api.EntityRoot;
import io.github.nextentity.core.api.Expression;
import io.github.nextentity.core.api.Expression.BooleanPathExpression;
import io.github.nextentity.core.api.Expression.EntityPathExpression;
import io.github.nextentity.core.api.Expression.NumberExpression;
import io.github.nextentity.core.api.Expression.NumberPathExpression;
import io.github.nextentity.core.api.Expression.OperatableExpression;
import io.github.nextentity.core.api.Expression.PathExpression;
import io.github.nextentity.core.api.Expression.Predicate;
import io.github.nextentity.core.api.Expression.StringExpression;
import io.github.nextentity.core.api.Expression.StringPathExpression;
import io.github.nextentity.core.api.ExpressionBuilder.AndOperator;
import io.github.nextentity.core.api.ExpressionBuilder.NumberOperator;
import io.github.nextentity.core.api.ExpressionBuilder.OrOperator;
import io.github.nextentity.core.api.ExpressionBuilder.PathOperator;
import io.github.nextentity.core.api.ExpressionBuilder.StringOperator;
import io.github.nextentity.core.api.ExpressionTree;
import io.github.nextentity.core.api.Operator;
import io.github.nextentity.core.api.Order;
import io.github.nextentity.core.api.Path;
import io.github.nextentity.core.api.Path.BooleanPath;
import io.github.nextentity.core.api.Path.NumberPath;
import io.github.nextentity.core.api.Path.StringPath;
import io.github.nextentity.core.api.Query.PredicateBuilder;
import io.github.nextentity.core.api.SortOrder;
import io.github.nextentity.core.expression.Attribute;
import io.github.nextentity.core.util.Iterators;
import io.github.nextentity.core.util.Lists;
import io.github.nextentity.core.util.Paths;
import lombok.experimental.Accessors;
import org.jetbrains.annotations.NotNull;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import static io.github.nextentity.core.Expressions.TypeExpressionImpl.EMPTY;

public class Expressions {

    public static <T, U> OperatableExpression<T, U> of(ExpressionTree expression) {
        return toTypedExpression(expression);
    }

    public static <T, U> Expression<T, U> of(U value) {
        return of(ExpressionTrees.literal(value));
    }

    public static <T, U> List<Expression<T, U>> ofList(U[] values) {
        return Arrays.stream(values)
                .map(Expressions::<T, U>of)
                .collect(Collectors.toList());
    }

    public static <T, U> List<Expression<T, U>> ofList(Iterable<? extends U> value) {
        return StreamSupport.stream(value.spliterator(), false)
                .map(Expressions::<T, U>of)
                .collect(Collectors.toList());
    }

    public static <T> Predicate<T> ofTrue() {
        return toTypedExpression(ExpressionTrees.TRUE);
    }

    public static <T, R> PathExpression<T, R> ofPath(Attribute column) {
        return toTypedExpression(column);
    }

    public static <T, R> EntityPathExpression<T, R> ofEntity(Attribute column) {
        return toTypedExpression(column);
    }

    public static <T> StringPathExpression<T> ofString(Attribute column) {
        return toTypedExpression(column);
    }

    public static <T, U extends Number> NumberPathExpression<T, U> ofNumber(Attribute column) {
        return toTypedExpression(column);
    }

    public static <T, R> OperatableExpression<T, R> ofBasic(ExpressionTree expression) {
        return toTypedExpression(expression);
    }

    public static <T> StringExpression<T> ofString(ExpressionTree expression) {
        return toTypedExpression(expression);
    }

    public static <T> Predicate<T> ofBoolean(ExpressionTree expression) {
        return toTypedExpression(expression);
    }

    public static <T> BooleanPathExpression<T> ofBoolean(Attribute expression) {
        return toTypedExpression(expression);
    }

    public static <T, U extends Number> NumberExpression<T, U> ofNumber(ExpressionTree expression) {
        return toTypedExpression(expression);
    }

    public static <T> Predicate<T> ofPredicate(ExpressionTree expression) {
        return toTypedExpression(expression);
    }

    static <T extends Expression<?, ?>> T toTypedExpression(ExpressionTree expression) {
        AbstractTypeExpression result;
        if (expression == null || expression instanceof AbstractTypeExpression) {
            result = (AbstractTypeExpression) expression;
        } else {
            result = new TypeExpressionImpl(expression.rootNode());
        }
        return TypeCastUtil.unsafeCast(result);
    }

    static final class TypeExpressionImpl implements AbstractTypeExpression {

        static final TypeExpressionImpl EMPTY = new TypeExpressionImpl(null);

        private final ExpressionNode expression;

        TypeExpressionImpl(ExpressionNode expression) {
            this.expression = expression;
        }

        @Override
        public ExpressionNode rootNode() {
            return expression;
        }
    }


    @Accessors(fluent = true)
    @SuppressWarnings("rawtypes")
    interface AbstractTypeExpression extends NumberPathExpression, StringPathExpression, BooleanPathExpression, EntityPathExpression {
        @Override
        default EntityRoot root() {
            return Paths.root();
        }

        @Override
        default NumberExpression count() {
            return toTypedExpression(ExpressionTrees.operate(rootNode(), Operator.COUNT));
        }

        @Override
        default NumberExpression countDistinct() {
            ExpressionNode distinct = ExpressionTrees.operate(rootNode(), Operator.DISTINCT);
            return toTypedExpression(ExpressionTrees.operate(distinct, Operator.COUNT));
        }

        @Override
        default Predicate eq(Object value) {
            return eq(Expressions.of(value));
        }

        @Override
        default Predicate eqIfNotNull(Object value) {
            return value == null ? operateNull() : eq(value);
        }

        @Override
        default Predicate eq(Expression value) {
            return operate(Operator.EQ, value);
        }

        @Override
        default Predicate ne(Object value) {
            return ne(Expressions.of(value));
        }

        @Override
        default Predicate neIfNotNull(Object value) {
            return value == null ? operateNull() : ne(value);
        }

        @Override
        default Predicate ne(Expression value) {
            return operate(Operator.NE, value);
        }

        @Override
        default Predicate in(@NotNull Expression expressions) {
            return operate(Operator.IN, expressions);
        }

        @Override
        default Predicate in(Object[] values) {
            List<Expression<?, ?>> collect = Arrays.stream(values)
                    .map(Expressions::of)
                    .collect(Collectors.toList());
            return in(collect);
        }

        @Override
        default Predicate in(@NotNull Collection values) {
            List<Expression<?, ?>> collect = ((Collection<?>) values).stream()
                    .map(Expressions::of)
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
            return operate(Operator.IS_NULL);
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
            return operate(Operator.IN, asTypeExpressions(expressions));
        }

        @Override
        default Predicate ge(Expression expression) {
            return operate(Operator.GE, expression);
        }

        @Override
        default Predicate gt(Expression expression) {
            return operate(Operator.GT, expression);
        }

        @Override
        default Predicate le(Expression expression) {
            return operate(Operator.LE, expression);
        }

        @Override
        default Predicate lt(Expression expression) {
            return operate(Operator.LT, expression);
        }

        @Override
        default Predicate between(Expression l, Expression r) {
            return operate(Operator.BETWEEN, Lists.of(l, r));
        }

        @Override
        default Predicate notBetween(Expression l, Expression r) {
            return not(between(l, r));
        }

        @Override
        default Order sort(SortOrder order) {
            return new OrderImpl(rootNode(), order);
        }

        @Override
        default Predicate geIfNotNull(Object value) {
            return value == null ? operateNull() : ge(Expressions.of(value));
        }

        @Override
        default Predicate gtIfNotNull(Object value) {
            return value == null ? operateNull() : gt(Expressions.of(value));
        }

        @Override
        default Predicate leIfNotNull(Object value) {
            return value == null ? operateNull() : le(Expressions.of(value));
        }

        @Override
        default Predicate ltIfNotNull(Object value) {
            return value == null ? operateNull() : lt(Expressions.of(value));
        }

        @Override
        default NumberExpression add(Expression expression) {
            return operate(Operator.ADD, expression);
        }

        @Override
        default NumberExpression subtract(Expression expression) {
            return operate(Operator.SUBTRACT, expression);
        }

        @Override
        default NumberExpression multiply(Expression expression) {
            return operate(Operator.MULTIPLY, expression);
        }

        @Override
        default NumberExpression divide(Expression expression) {
            return operate(Operator.DIVIDE, expression);
        }

        @Override
        default NumberExpression mod(Expression expression) {
            return operate(Operator.MOD, expression);
        }

        @Override
        default NumberExpression sum() {
            return operate(Operator.SUM);
        }

        @Override
        default NumberExpression avg() {
            return operate(Operator.AVG);
        }

        @Override
        default NumberExpression max() {
            return operate(Operator.MAX);
        }

        @Override
        default NumberExpression min() {
            return operate(Operator.MIN);
        }

        @Override
        default Predicate like(String value) {
            return operate(Operator.LIKE, ExpressionTrees.of(value));
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
            return operate(Operator.LOWER);
        }

        @Override
        default StringExpression upper() {
            return operate(Operator.UPPER);
        }

        @Override
        default StringExpression substring(int offset, int length) {
            return operate0(Operator.SUBSTRING, Lists.of(ExpressionTrees.of(offset), ExpressionTrees.of(length)));
        }

        @Override
        default StringExpression trim() {
            return operate(Operator.TRIM);
        }

        @Override
        default NumberExpression length() {
            return operate(Operator.LENGTH);
        }

        @Override
        default PathOperator and(Path path) {
            return ExpressionBuilders.ofPath(of(path).asBasic(), this::and);
        }

        @NotNull
        default AbstractTypeExpression and(OperatableExpression<?, ?> basicExpression) {
            return basicExpression == null ? this : operate(Operator.AND, basicExpression);
        }

        @NotNull
        default AbstractTypeExpression or(OperatableExpression<?, ?> basicExpression) {
            return basicExpression == null ? this : operate(Operator.OR, basicExpression);
        }

        @NotNull
        static AbstractTypeExpression of(Path path) {
            return toTypedExpression(ExpressionTrees.of(path));
        }

        @Override
        default NumberOperator and(NumberPath path) {
            return ExpressionBuilders.ofNumber(of(path).asNumber(), this::and);
        }

        @Override
        default StringOperator and(StringPath path) {
            return ExpressionBuilders.ofString(of(path).asString(), this::and);
        }

        @Override
        default AndOperator andIf(boolean predicate, PredicateBuilder predicateBuilder) {
            return predicate ? and(((PredicateBuilder<?>) predicateBuilder).build(TypeCastUtil.cast(root()))) : this;
        }

        @Override
        default PathOperator or(Path path) {
            return ExpressionBuilders.ofPath(of(path).asBasic(), this::or);
        }

        @Override
        default NumberOperator or(NumberPath path) {
            return ExpressionBuilders.ofNumber(of(path).asNumber(), this::or);
        }


        @Override
        default StringOperator or(StringPath path) {
            return ExpressionBuilders.ofString(of(path).asString(), this::or);
        }

        @Override
        default OrOperator orIf(boolean predicate, PredicateBuilder predicateBuilder) {
            return predicate ? or(((PredicateBuilder<?>) predicateBuilder).build(TypeCastUtil.cast(root()))) : this;
        }

        @Override
        default Predicate or(Expression predicate) {
            return operate(Operator.OR, predicate);
        }

        @Override
        default Predicate and(Expression expression) {
            return operate(Operator.AND, expression);
        }

        @Override
        default EntityPathExpression get(Path path) {
            // PathChain expression = (PathChain) Paths.get((Path<?, ?>) path).rootNode();
            String name = ExpressionTrees.columnName(path);
            return toTypedExpression(((Attribute) rootNode()).get(name));
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
            return operate(Operator.NOT);
        }

        @Override
        default Predicate and(Expression[] predicate) {
            return operate(Operator.AND, Arrays.asList(predicate));
        }

        @Override
        default Predicate or(Expression[] predicate) {
            return operate(Operator.OR, Arrays.asList(predicate));
        }

        @Override
        default Predicate and(Iterable predicates) {
            return operate(Operator.AND, TypeCastUtil.cast(Iterators.toList((Iterable<?>) predicates)));
        }

        @Override
        default Predicate toPredicate() {
            return Expressions.ofPredicate(this);
        }

        @Override
        default Predicate or(Iterable predicates) {
            return operate(Operator.OR, TypeCastUtil.cast(Iterators.toList((Iterable<?>) predicates)));
        }

        default AbstractTypeExpression get0(Path<?, ?> path) {
            PathExpression<?, ?> pathExpression = Paths.get((Path<?, ?>) path);
            return get0(pathExpression);
        }

        @NotNull
        default AbstractTypeExpression get0(PathExpression<?, ?> pathExpression) {
            Attribute expression = (Attribute) pathExpression.rootNode();
            ExpressionNode expr = rootNode();
            return toTypedExpression(((Attribute) expr).get(expression));
        }

        default AbstractTypeExpression not(Expression<?, ?> expression) {
            ExpressionNode operate = ExpressionTrees.operate(expression.rootNode(), Operator.NOT);
            return toTypedExpression(operate);
        }

        @NotNull
        default AbstractTypeExpression operate(Operator operator, ExpressionTree expression) {
            if (rootNode() == null) {
                return toTypedExpression(expression);
            }
            return toTypedExpression(ExpressionTrees.operate(rootNode(), operator, expression.rootNode()));
        }

        @NotNull
        default AbstractTypeExpression operate(Operator operator, List<? extends ExpressionTree> expressions) {
            List<ExpressionTree> list = expressions.stream().map(ExpressionTree::rootNode).collect(Collectors.toList());
            return operate0(operator, list);
        }

        @NotNull
        default AbstractTypeExpression operate0(Operator operator, List<ExpressionTree> list) {
            return toTypedExpression(ExpressionTrees.operate(rootNode(), operator, list));
        }

        @NotNull
        default AbstractTypeExpression operate(Operator operator) {
            return toTypedExpression(ExpressionTrees.operate(rootNode(), operator));
        }

        @NotNull
        static Predicate operateNull() {
            return EMPTY;
        }

        @NotNull
        default StringExpression<?> asString() {
            return this;
        }

        default OperatableExpression<?, ?> asBasic() {
            return this;
        }

        default NumberExpression<?, ?> asNumber() {
            return this;
        }

        default List<? extends Expression<?, ?>> asTypeExpressions(List<?> list) {
            return TypeCastUtil.cast(list);
        }


        @Override
        default Predicate likeIfNotEmpty(String value) {
            return value == null || value.isEmpty() ? operateNull() : like(value);
        }

        @Override
        default Predicate notLikeIfNotEmpty(String value) {
            return value == null || value.isEmpty() ? operateNull() : notLike(value);
        }

        @Override
        default Predicate eqIfNotEmpty(String value) {
            return value == null || value.isEmpty() ? operateNull() : eq(value);
        }

    }
}
