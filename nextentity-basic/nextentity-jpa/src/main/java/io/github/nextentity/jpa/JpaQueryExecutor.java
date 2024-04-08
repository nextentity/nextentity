package io.github.nextentity.jpa;

import io.github.nextentity.core.ExpressionTrees;
import io.github.nextentity.core.QueryExecutor;
import io.github.nextentity.core.Tuples;
import io.github.nextentity.core.TypeCastUtil;
import io.github.nextentity.core.api.ExpressionTree;
import io.github.nextentity.core.api.ExpressionTree.Column;
import io.github.nextentity.core.api.ExpressionTree.ExpressionNode;
import io.github.nextentity.core.api.ExpressionTree.Operation;
import io.github.nextentity.core.api.ExpressionTree.QueryStructure;
import io.github.nextentity.core.api.ExpressionTree.QueryStructure.From;
import io.github.nextentity.core.api.ExpressionTree.QueryStructure.From.FromSubQuery;
import io.github.nextentity.core.api.ExpressionTree.QueryStructure.Order;
import io.github.nextentity.core.api.ExpressionTree.QueryStructure.Selection;
import io.github.nextentity.core.api.ExpressionTree.QueryStructure.Selection.EntitySelected;
import io.github.nextentity.core.api.ExpressionTree.QueryStructure.Selection.MultiSelected;
import io.github.nextentity.core.api.ExpressionTree.QueryStructure.Selection.ProjectionSelected;
import io.github.nextentity.core.api.ExpressionTree.QueryStructure.Selection.SingleSelected;
import io.github.nextentity.core.util.Lists;
import io.github.nextentity.core.api.SortOrder;
import io.github.nextentity.core.meta.Attribute;
import io.github.nextentity.core.meta.Metamodel;
import io.github.nextentity.core.meta.Projection;
import io.github.nextentity.core.meta.ProjectionAttribute;
import io.github.nextentity.core.meta.SubSelectType;
import io.github.nextentity.core.reflect.InstanceConstructor;
import io.github.nextentity.core.reflect.ReflectUtil;
import jakarta.persistence.EntityManager;
import jakarta.persistence.LockModeType;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Fetch;
import jakarta.persistence.criteria.JoinType;
import jakarta.persistence.criteria.Root;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

@SuppressWarnings("PatternVariableCanBeUsed")
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
        Selection selected = queryStructure.select();
        if (selected instanceof SingleSelected) {
            SingleSelected singleSelected = (SingleSelected) selected;
            List<Object[]> objectsList = getObjectsList(queryStructure, Lists.of(singleSelected.expression()));
            List<Object> result = objectsList.stream().map(objects -> objects[0]).collect(Collectors.toList());
            return TypeCastUtil.cast(result);
        } else if (selected instanceof MultiSelected) {
            MultiSelected multiSelected = (MultiSelected) selected;
            List<Object[]> objectsList = getObjectsList(queryStructure, multiSelected.expressions());
            return TypeCastUtil.cast(objectsList.stream().map(Tuples::of).collect(Collectors.toList()));
        } else if (queryStructure.select() instanceof EntitySelected) {
            List<?> resultList = getEntityResultList(queryStructure);
            return TypeCastUtil.cast(resultList);
        } else if (queryStructure.select() instanceof ProjectionSelected) {
            Class<?> resultType = queryStructure.select().resultType();
            Projection projection = metamodel
                    .getProjection(queryStructure.from().type(), resultType);
            Collection<? extends ProjectionAttribute> attributes = projection.attributes();
            List<Column> columns = attributes.stream()
                    .map(ProjectionAttribute::entityAttribute)
                    .map(Attribute::column)
                    .collect(Collectors.toList());
            List<Object[]> objectsList = getObjectsList(queryStructure, columns);
            InstanceConstructor extractor = ReflectUtil.getRowInstanceConstructor(attributes, resultType);
            return objectsList.stream()
                    .map(extractor::newInstance)
                    .map(TypeCastUtil::<T>unsafeCast)
                    .collect(Collectors.toList());
        } else {
            throw new IllegalStateException();
        }
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

    private boolean hasSubQuery(Collection<? extends ExpressionTree> expressions) {
        for (ExpressionTree operand : expressions) {
            if (hasSubQuery(operand)) {
                return true;
            }
        }
        return false;
    }

    private boolean hasSubQuery(ExpressionTree expression) {
        ExpressionNode tree = expression.rootNode();
        if (tree instanceof QueryStructure) {
            return true;
        }
        if (tree instanceof Operation) {
            List<? extends ExpressionTree> expressions = ((Operation) tree).operands();
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

    private List<Object[]> getObjectsList(@NotNull QueryStructure structure, List<? extends ExpressionNode> columns) {
        CriteriaBuilder cb = entityManager.getCriteriaBuilder();
        CriteriaQuery<?> query = cb.createQuery(Object[].class);
        Root<?> root = query.from(structure.from().type());
        return new ObjectArrayBuilder(root, cb, query, structure, columns).getResultList();
    }

    class ObjectArrayBuilder extends Builder {

        private final List<? extends ExpressionNode> selects;

        public ObjectArrayBuilder(Root<?> root,
                                  CriteriaBuilder cb,
                                  CriteriaQuery<?> query,
                                  QueryStructure structure,
                                  List<? extends ExpressionNode> selects) {
            super(root, cb, query, structure);
            this.selects = selects;
        }

        public List<Object[]> getResultList() {
            return super.getResultList()
                    .stream()
                    .map(it -> {
                        if (it instanceof Object[]) {
                            return (Object[]) it;
                        }
                        return new Object[]{it};
                    })
                    .collect(Collectors.toList());
        }

        @Override
        protected TypedQuery<?> getTypedQuery() {
            CriteriaQuery<?> select = query.multiselect(
                    selects.stream()
                            .map(this::toExpression)
                            .collect(Collectors.toList())
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
                List<jakarta.persistence.criteria.Order> orders = orderBy.stream()
                        .map(o -> o.order() == SortOrder.DESC
                                ? cb.desc(toExpression(o.expression()))
                                : cb.asc(toExpression(o.expression())))
                        .collect(Collectors.toList());
                query.orderBy(orders);
            }
        }

        protected void setWhere(ExpressionNode where) {
            if (!ExpressionTrees.isNullOrTrue(where)) {
                query.where(toPredicate(where));
            }
        }

        protected void setGroupBy(List<? extends ExpressionNode> groupBy) {
            if (groupBy != null && !groupBy.isEmpty()) {
                List<jakarta.persistence.criteria.Expression<?>> grouping = groupBy.stream().map(this::toExpression).collect(Collectors.toList());
                query.groupBy(grouping);
            }
        }

        protected void setHaving(ExpressionNode having) {
            if (!ExpressionTrees.isNullOrTrue(having)) {
                query.having(toPredicate(having));
            }
        }

        protected void setFetch(List<? extends Column> fetchPaths) {
            if (fetchPaths != null) {
                for (Column path : fetchPaths) {
                    Fetch<?, ?> fetch = null;
                    for (int i = 0; i < path.size(); i++) {
                        Fetch<?, ?> cur = fetch;
                        String stringPath = path.get(i);
                        Column sub = path.subLength(i + 1);
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
            setDistinct(structure.select());
            setFetch(structure.fetch());
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

        private void setDistinct(Selection select) {
            query.distinct(select.distinct());
        }

        protected abstract TypedQuery<?> getTypedQuery();

    }

}
