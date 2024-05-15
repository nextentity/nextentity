package io.github.nextentity.jpa;

import io.github.nextentity.api.Expression;
import io.github.nextentity.api.model.Order;
import io.github.nextentity.api.SortOrder;
import io.github.nextentity.core.QueryExecutor;
import io.github.nextentity.core.TypeCastUtil;
import io.github.nextentity.core.expression.EntityPath;
import io.github.nextentity.core.expression.Operation;
import io.github.nextentity.core.expression.QueryStructure;
import io.github.nextentity.core.expression.QueryStructure.From;
import io.github.nextentity.core.expression.QueryStructure.From.FromSubQuery;
import io.github.nextentity.core.expression.QueryStructure.Selected;
import io.github.nextentity.core.expression.QueryStructure.Selected.SelectEntity;
import io.github.nextentity.core.expression.impl.ExpressionImpls;
import io.github.nextentity.core.meta.Metamodel;
import io.github.nextentity.core.meta.SubSelectType;
import io.github.nextentity.core.util.ImmutableList;
import io.github.nextentity.jdbc.QueryContext;
import javax.persistence.EntityManager;
import javax.persistence.LockModeType;
import javax.persistence.TypedQuery;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Fetch;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Root;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.List;

import static io.github.nextentity.core.reflect.schema.InstanceFactory.PrimitiveFactory;

public class JpaQueryExecutor implements QueryExecutor {

    private final EntityManager entityManager;
    private final Metamodel metamodel;
    private final QueryExecutor nativeQueryExecutor;

    public JpaQueryExecutor(EntityManager entityManager, Metamodel metamodel, QueryExecutor nativeQueryExecutor) {
        this.entityManager = entityManager;
        this.metamodel = metamodel;
        this.nativeQueryExecutor = nativeQueryExecutor;
    }

    @Override
    public <T> List<T> getList(@NotNull QueryStructure queryStructure) {
        if (requiredNativeQuery(queryStructure)) {
            return nativeQueryExecutor.getList(queryStructure);
        }
        Selected selected = queryStructure.select();
        if (selected instanceof SelectEntity) {
            List<?> resultList = getEntityResultList(queryStructure);
            return TypeCastUtil.cast(resultList);
        }
        QueryContext context = new QueryContext(queryStructure, metamodel, false);
        List<Object[]> objectsList = getObjectsList(queryStructure, context.getConstructor().primitives());
        List<Object> result = objectsList.stream()
                .map(objects -> {
                    JpaArguments arguments = new JpaArguments(
                            objects, null, null);
                    return context.construct(arguments);
                })
                .collect(ImmutableList.collector(objectsList.size()));
        return TypeCastUtil.cast(result);
    }

    private boolean requiredNativeQuery(@NotNull QueryStructure queryStructure) {
        From from = queryStructure.from();
        return from instanceof FromSubQuery
               || metamodel.getEntity(from.type()) instanceof SubSelectType
               || hasSubQuery(queryStructure);
    }

    private boolean hasSubQuery(QueryStructure queryStructure) {
        return hasSubQuery(queryStructure.where())
               || hasSubQuery(queryStructure.groupBy())
               || hasSubQuery(queryStructure.orderBy())
               || hasSubQuery(queryStructure.having());
    }

    private boolean hasSubQuery(List<? extends Order<?>> orders) {
        for (Order<?> order : orders) {
            if (hasSubQuery(order.expression())) {
                return true;
            }
        }
        return false;
    }

    private boolean hasSubQuery(Collection<? extends Expression> expressions) {
        for (Expression operand : expressions) {
            if (hasSubQuery(operand)) {
                return true;
            }
        }
        return false;
    }

    private boolean hasSubQuery(Expression expression) {
        if (expression instanceof QueryStructure) {
            return true;
        }
        if (expression instanceof Operation) {
            List<? extends Expression> expressions = ((Operation) expression).operands();
            return hasSubQuery(expressions);
        }
        return false;
    }


    private List<?> getEntityResultList(@NotNull QueryStructure structure) {
        CriteriaBuilder cb = entityManager.getCriteriaBuilder();
        CriteriaQuery<?> query = cb.createQuery(structure.from().type());
        Root<?> root = query.from(structure.from().type());
        return new EntityBuilder(root, cb, query, structure).getResultList();
    }

    private List<Object[]> getObjectsList(@NotNull QueryStructure structure, List<? extends PrimitiveFactory> columns) {
        CriteriaBuilder cb = entityManager.getCriteriaBuilder();
        CriteriaQuery<?> query = cb.createQuery(Object[].class);
        Root<?> root = query.from(structure.from().type());
        return new ObjectArrayBuilder(root, cb, query, structure, columns).getResultList();
    }

    class ObjectArrayBuilder extends Builder {

        private final List<? extends PrimitiveFactory> selects;

        public ObjectArrayBuilder(Root<?> root,
                                  CriteriaBuilder cb,
                                  CriteriaQuery<?> query,
                                  QueryStructure structure,
                                  List<? extends PrimitiveFactory> selects) {
            super(root, cb, query, structure);
            this.selects = selects;
        }

        public List<Object[]> getResultList() {
            List<?> resultList = super.getResultList();
            return resultList
                    .stream()
                    .map(it -> {
                        if (it instanceof Object[]) {
                            return (Object[]) it;
                        }
                        return new Object[]{it};
                    })
                    .collect(ImmutableList.collector(resultList.size()));
        }

        @Override
        protected TypedQuery<?> getTypedQuery() {
            CriteriaQuery<?> select = query.multiselect(
                    selects.stream()
                            .map(PrimitiveFactory::expression)
                            .map(this::toExpression)
                            .collect(ImmutableList.collector(selects.size()))
            );

            return entityManager.createQuery(select);
        }

    }

    class EntityBuilder extends Builder {
        public EntityBuilder(Root<?> root, CriteriaBuilder cb, CriteriaQuery<?> query, QueryStructure structure) {
            super(root, cb, query, structure);
        }

        @Override
        protected TypedQuery<?> getTypedQuery() {
            return entityManager.createQuery(query);
        }

    }

    protected static abstract class Builder extends JpaExpressionBuilder {
        protected final QueryStructure structure;
        protected final CriteriaQuery<?> query;

        public Builder(Root<?> root, CriteriaBuilder cb, CriteriaQuery<?> query, QueryStructure structure) {
            super(root, cb);
            this.structure = structure;
            this.query = query;
        }

        protected void setOrderBy(List<? extends Order<?>> orderBy) {
            if (orderBy != null && !orderBy.isEmpty()) {
                List<javax.persistence.criteria.Order> orders = orderBy.stream()
                        .map(o -> o.order() == SortOrder.DESC
                                ? cb.desc(toExpression(o.expression()))
                                : cb.asc(toExpression(o.expression())))
                        .collect(ImmutableList.collector(orderBy.size()));
                query.orderBy(orders);
            }
        }

        protected void setWhere(Expression where) {
            if (!ExpressionImpls.isNullOrTrue(where)) {
                query.where(toPredicate(where));
            }
        }

        protected void setGroupBy(List<? extends Expression> groupBy) {
            if (groupBy != null && !groupBy.isEmpty()) {
                List<javax.persistence.criteria.Expression<?>> grouping = groupBy.stream()
                        .map(this::toExpression)
                        .collect(ImmutableList.collector(groupBy.size()));
                query.groupBy(grouping);
            }
        }

        protected void setHaving(Expression having) {
            if (!ExpressionImpls.isNullOrTrue(having)) {
                query.having(toPredicate(having));
            }
        }

        protected void setFetch(Collection<? extends EntityPath> fetchPaths) {
            if (fetchPaths != null) {
                for (EntityPath path : fetchPaths) {
                    Fetch<?, ?> fetch = null;
                    for (int i = 0; i < path.deep(); i++) {
                        Fetch<?, ?> cur = fetch;
                        String stringPath = path.get(i);
                        EntityPath sub = path.subLength(i + 1);
                        fetch = (Fetch<?, ?>) fetched.computeIfAbsent(sub, k -> {
                            if (cur == null) {
                                return root.fetch(stringPath, JoinType.LEFT);
                            } else {
                                return cur.fetch(stringPath, JoinType.LEFT);
                            }
                        });
                    }
                }
            }
        }

        protected List<?> getResultList() {
            Selected select = structure.select();
            setDistinct(select);
            if (select instanceof SelectEntity) {
                Collection<? extends EntityPath> attributes = ((SelectEntity) select)
                        .fetch();
                setFetch(attributes);
            }
            setWhere(structure.where());
            setGroupBy(structure.groupBy());
            setHaving(structure.having());
            setOrderBy(structure.orderBy());
            TypedQuery<?> objectsQuery = getTypedQuery();
            Integer offset = structure.offset();
            if (offset != null && offset > 0) {
                objectsQuery = objectsQuery.setFirstResult(offset);
            }
            Integer maxResult = structure.limit();
            if (maxResult != null && maxResult > 0) {
                objectsQuery = objectsQuery.setMaxResults(maxResult);
            }
            LockModeType lockModeType = LockModeTypeAdapter.of(structure.lockType());
            if (lockModeType != null) {
                objectsQuery.setLockMode(lockModeType);
            }
            return objectsQuery.getResultList();
        }

        private void setDistinct(Selected select) {
            query.distinct(select.distinct());
        }

        protected abstract TypedQuery<?> getTypedQuery();

    }

}
