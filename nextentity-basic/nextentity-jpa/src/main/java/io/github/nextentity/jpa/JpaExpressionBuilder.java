package io.github.nextentity.jpa;

import io.github.nextentity.core.TypeCastUtil;
import io.github.nextentity.core.api.ExpressionTree;
import io.github.nextentity.core.expression.Attribute;
import io.github.nextentity.core.api.ExpressionTree.ExpressionNode;
import io.github.nextentity.core.expression.Literal;
import io.github.nextentity.core.expression.Operation;
import io.github.nextentity.core.api.Operator;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.FetchParent;
import jakarta.persistence.criteria.From;
import jakarta.persistence.criteria.Join;
import jakarta.persistence.criteria.JoinType;
import jakarta.persistence.criteria.Path;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;
import org.jetbrains.annotations.NotNull;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

@SuppressWarnings("PatternVariableCanBeUsed")
public class JpaExpressionBuilder {

    protected final Root<?> root;

    protected final CriteriaBuilder cb;

    protected final Map<Attribute, FetchParent<?, ?>> fetched = new HashMap<>();

    public JpaExpressionBuilder(Root<?> root, CriteriaBuilder cb) {
        this.root = root;
        this.cb = cb;
    }

    public jakarta.persistence.criteria.Expression<?> toExpression(ExpressionNode expression) {
        if (expression instanceof Literal) {
            Literal cv = (Literal) expression;
            return cb.literal(cv.value());
        }
        if (expression instanceof Attribute) {
            Attribute pv = (Attribute) expression;
            return getPath(pv);
        }
        if (expression instanceof Operation) {
            Operation ov = (Operation) expression;
            Operator operator = ov.operator();
            jakarta.persistence.criteria.Expression<?> e0 = toExpression(ov.firstOperand());
            ExpressionNode e1 = ov.secondOperand();
            ExpressionNode e2 = ov.thirdOperand();
            switch (operator) {
                case NOT:
                    return toPredicate(e0).not();
                case AND: {
                    Predicate[] predicates = toPredicateArray(ov.operands());
                    return cb.and(predicates);
                }
                case OR: {
                    Predicate[] predicates = toPredicateArray(ov.operands());
                    return cb.or(predicates);
                }
                case EQ: {
                    return cb.equal(e0, toExpression(e1));
                }
                case NE: {
                    return cb.notEqual(e0, toExpression(e1));
                }
                case GT: {
                    return cb.gt(cast(e0), cast(toExpression(e1)));
                }
                case GE: {
                    return cb.ge(cast(e0), cast(toExpression(e1)));
                }
                case LT: {
                    return cb.lt(cast(e0), cast(toExpression(e1)));
                }
                case LE: {
                    return cb.le(cast(e0), cast(toExpression(e1)));
                }
                case LIKE: {
                    return cb.like(cast(e0), cast(toExpression(e1)));
                }
                case IS_NULL:
                    return cb.isNull(e0);
                case IS_NOT_NULL:
                    return cb.isNotNull(e0);
                case IN: {
                    List<? extends ExpressionNode> operands = ov.operands();
                    if (operands.size() <= 1) {
                        return cb.literal(false);
                    } else {
                        CriteriaBuilder.In<Object> in = cb.in(e0);
                        for (int i = 1; i < operands.size(); i++) {
                            ExpressionNode arg = operands.get(i);
                            in = in.value(toExpression(arg));
                        }
                        return in;
                    }
                }
                case BETWEEN: {
                    return cb.between(cast(e0), cast(toExpression(e1)), cast(toExpression(e2)));
                }
                case LOWER:
                    return cb.lower(cast(e0));
                case UPPER:
                    return cb.upper(cast(e0));
                case SUBSTRING: {
                    List<? extends ExpressionTree> operands = ov.operands();
                    if (operands.size() == 2) {
                        return cb.substring(cast(e0), cast(toExpression(e1)));
                    } else if (operands.size() == 3) {
                        return cb.substring(cast(e0), cast(toExpression(e1)), cast(toExpression(e2)));
                    } else {
                        throw new IllegalArgumentException("argument length error");
                    }
                }
                case TRIM:
                    return cb.trim(cast(e0));
                case LENGTH:
                    return cb.length(cast(e0));
                case ADD: {
                    return cb.sum(cast(e0), cast(toExpression(e1)));
                }
                case SUBTRACT: {
                    return cb.diff(cast(e0), cast(toExpression(e1)));
                }
                case MULTIPLY: {
                    return cb.prod(cast(e0), cast(toExpression(e1)));
                }
                case DIVIDE: {
                    return cb.quot(cast(e0), cast(toExpression(e1)));
                }
                case MOD: {
                    return cb.mod(cast(e0), cast(toExpression(e1)));
                }
                case NULLIF: {
                    return cb.nullif(e0, toExpression(e1));
                }
                case IF_NULL: {
                    return cb.coalesce(e0, toExpression(e1));
                }
                case MIN:
                    return cb.min(cast(e0));
                case MAX:
                    return cb.max(cast(e0));
                case COUNT:
                    return cb.count(cast(e0));
                case AVG:
                    return cb.avg(cast(e0));
                case SUM:
                    return cb.sum(cast(e0));
                default:
                    throw new UnsupportedOperationException(operator.name());
            }
        } else {
            throw new UnsupportedOperationException("unknown expression type " + expression.getClass());
        }
    }

    public Predicate toPredicate(ExpressionNode expression) {
        return toPredicate(toExpression(expression));
    }

    private Predicate toPredicate(jakarta.persistence.criteria.Expression<?> result) {
        if (result instanceof Predicate) {
            return (Predicate) result;
        }
        return cb.isTrue(cast(result));
    }

    @NotNull
    private Predicate[] toPredicateArray(List<? extends ExpressionNode> operands) {
        return operands.stream()
                .map(this::toPredicate)
                .toArray(Predicate[]::new);
    }

    public static <T> jakarta.persistence.criteria.Expression<T> cast(jakarta.persistence.criteria.Expression<?> expression) {
        return unsafeCast(expression);
    }

    public static <T> T unsafeCast(Object o) {
        return TypeCastUtil.unsafeCast(o);
    }

    protected Path<?> getPath(Attribute column) {
        From<?, ?> r = root;
        int size = column.deep();
        for (int i = 0; i < size; i++) {
            String s = column.get(i);
            if (i != size - 1) {
                Attribute offset = column.subLength(i + 1);
                r = join(offset);
            } else {
                return r.get(s);
            }
        }

        return r;
    }

    private Join<?, ?> join(Attribute column) {
        return (Join<?, ?>) fetched.compute(column, (k, v) -> {
            if (v instanceof Join<?, ?>) {
                return v;
            } else {
                From<?, ?> r = root;
                for (String s : column) {
                    r = r.join(s, JoinType.LEFT);
                }
                return r;
            }
        });
    }
}