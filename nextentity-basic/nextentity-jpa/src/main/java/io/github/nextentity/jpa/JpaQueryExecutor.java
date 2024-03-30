package io.github.nextentity.jpa;

import io.github.nextentity.core.AbstractQueryExecutor;
import io.github.nextentity.core.Expressions;
import io.github.nextentity.core.Tuples;
import io.github.nextentity.core.TypeCastUtil;
import io.github.nextentity.core.api.Column;
import io.github.nextentity.core.api.Expression;
import io.github.nextentity.core.api.From;
import io.github.nextentity.core.api.From.FromSubQuery;
import io.github.nextentity.core.api.Lists;
import io.github.nextentity.core.api.Operation;
import io.github.nextentity.core.api.Order;
import io.github.nextentity.core.api.Order.SortOrder;
import io.github.nextentity.core.api.QueryExecutor;
import io.github.nextentity.core.api.QueryStructure;
import io.github.nextentity.core.api.Selection;
import io.github.nextentity.core.api.Selection.EntitySelected;
import io.github.nextentity.core.api.Selection.MultiSelected;
import io.github.nextentity.core.api.Selection.ProjectionSelected;
import io.github.nextentity.core.api.Selection.SingleSelected;
import io.github.nextentity.core.api.SubQuery;
import io.github.nextentity.core.converter.TypeConverter;
import io.github.nextentity.core.meta.Attribute;
import io.github.nextentity.core.meta.Metamodel;
import io.github.nextentity.core.meta.Projection;
import io.github.nextentity.core.meta.ProjectionAttribute;
import io.github.nextentity.core.meta.SubSelectType;
import io.github.nextentity.core.reflect.InstanceConstructor;
import io.github.nextentity.core.reflect.ReflectUtil;
import io.github.nextentity.jdbc.JdbcQueryExecutor.QuerySqlBuilder;
import org.jetbrains.annotations.NotNull;

import javax.persistence.EntityManager;
import javax.persistence.LockModeType;
import javax.persistence.TypedQuery;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Fetch;
import javax.persistence.criteria.JoinType;
import javax.persistence.criteria.Root;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

public class JpaQueryExecutor implements AbstractQueryExecutor {

    private final EntityManager entityManager;
    private final Metamodel metamodel;
    private final QueryExecutor nativeQueryExecutor;

    public JpaQueryExecutor(EntityManager entityManager, Metamodel metamodel, QueryExecutor nativeQueryExecutor) {
        this.entityManager = entityManager;
        this.metamodel = metamodel;
        this.nativeQueryExecutor = nativeQueryExecutor;
    }

    public JpaQueryExecutor(EntityManager entityManager, Metamodel metamodel, QuerySqlBuilder querySqlBuilder, TypeConverter converter) {
        this.entityManager = entityManager;
        this.metamodel = metamodel;
        this.nativeQueryExecutor = new JpaNativeQueryExecutor(querySqlBuilder, entityManager, metamodel, converter);
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

    private boolean hasSubQuery(Collection<? extends Expression> expressions) {
        for (Expression operand : expressions) {
            if (hasSubQuery(operand)) {
                return true;
            }
        }
        return false;
    }

    private boolean hasSubQuery(Expression expression) {
        if (expression instanceof SubQuery) {
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

    private List<Object[]> getObjectsList(@NotNull QueryStructure structure, List<? extends Expression> columns) {
        CriteriaBuilder cb = entityManager.getCriteriaBuilder();
        CriteriaQuery<?> query = cb.createQuery(Object[].class);
        Root<?> root = query.from(structure.from().type());
        return new ObjectArrayBuilder(root, cb, query, structure, columns).getResultList();
    }

    class ObjectArrayBuilder extends Builder {

        private final List<? extends Expression> selects;

        public ObjectArrayBuilder(Root<?> root,
                                  CriteriaBuilder cb,
                                  CriteriaQuery<?> query,
                                  QueryStructure structure,
                                  List<? extends Expression> selects) {
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

    protected static abstract class Builder extends PredicateBuilder {
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
                        .collect(Collectors.toList());
                query.orderBy(orders);
            }
        }

        protected void setWhere(Expression where) {
            if (!Expressions.isNullOrTrue(where)) {
                query.where(toPredicate(where));
            }
        }

        protected void setGroupBy(List<? extends Expression> groupBy) {
            if (groupBy != null && !groupBy.isEmpty()) {
                List<javax.persistence.criteria.Expression<?>> grouping = groupBy.stream().map(this::toExpression).collect(Collectors.toList());
                query.groupBy(grouping);
            }
        }

        protected void setHaving(Expression having) {
            if (!Expressions.isNullOrTrue(having)) {
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
                        Column sub = subPaths(path, i + 1);
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
