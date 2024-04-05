package io.github.nextentity.jpa;

import io.github.nextentity.core.api.Expression.ExpressionTree;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;

public class PredicateBuilder extends ExpressionBuilder {

    public PredicateBuilder(Root<?> root, CriteriaBuilder cb) {
        super(root, cb);
    }

    public Predicate toPredicate(ExpressionTree expression) {
        jakarta.persistence.criteria.Expression<?> result = toExpression(expression);
        if (result instanceof Predicate) {
            return (Predicate) result;
        }
        return cb.isTrue(cast(result));
    }

}