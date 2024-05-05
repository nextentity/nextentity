package io.github.nextentity.jdbc;

import io.github.nextentity.api.Expression;
import io.github.nextentity.core.ExpressionTypeResolver;
import io.github.nextentity.core.expression.EntityPath;
import io.github.nextentity.core.expression.QueryStructure;
import io.github.nextentity.core.expression.QueryStructure.From;
import io.github.nextentity.core.expression.QueryStructure.From.FromEntity;
import io.github.nextentity.core.expression.QueryStructure.Selected;
import io.github.nextentity.core.expression.QueryStructure.Selected.SelectEntity;
import io.github.nextentity.core.expression.QueryStructure.Selected.SelectProjection;
import io.github.nextentity.core.meta.AssociationAttribute;
import io.github.nextentity.core.meta.BasicAttribute;
import io.github.nextentity.core.meta.EntityType;
import io.github.nextentity.core.meta.Metamodel;
import io.github.nextentity.core.reflect.Arguments;
import io.github.nextentity.core.reflect.InstanceFactories;
import io.github.nextentity.core.reflect.schema.InstanceFactory;
import io.github.nextentity.core.util.ImmutableList;
import lombok.Data;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;

/**
 * @author HuangChengwei
 * @since 2024/4/20 下午12:03
 */
@Data
public class QueryContext {

    protected final QueryStructure structure;
    protected final Metamodel metamodel;
    protected final EntityType entityType;
    protected final boolean expandReferencePath;
    protected final InstanceFactory constructor;

    public QueryContext(QueryStructure structure, Metamodel metamodel, boolean expandObjectAttribute) {
        this.structure = structure;
        this.metamodel = metamodel;
        this.expandReferencePath = expandObjectAttribute;
        From from = structure.from();
        this.entityType = from instanceof FromEntity ? metamodel.getEntity(from.type()) : null;
        this.constructor = getSelectedConstruct();
    }

    private @NotNull InstanceFactory getSelectedConstruct() {
        Selected select = structure.select();
        if (select instanceof SelectEntity) {
            Collection<? extends EntityPath> fetch = ((SelectEntity) select).fetch();
            if (fetch == null || fetch.isEmpty() || !expandReferencePath) {
                return entityType.getInstanceFactory();
            } else {
                return InstanceFactories.fetch(entityType, fetch);
            }
        } else if (select instanceof SelectProjection) {
            return entityType.getProjection(select.type()).getInstanceFactory();
        } else if (select instanceof Selected.SelectPrimitive selectPrimitive) {
            return newPrimitiveFactory(selectPrimitive);
        } else if (select instanceof Selected.SelectArray selectArray) {
            ImmutableList<InstanceFactory> factories = selectArray.items().stream()
                    .map(this::newPrimitiveFactory)
                    .collect(ImmutableList.collector(selectArray.items().size()));
            return new InstanceFactories.ArrayFactoryImpl(factories);
        }
        throw new IllegalArgumentException("Unknown select type: " + select.getClass().getName());
    }

    private InstanceFactory newPrimitiveFactory(Selected.SelectPrimitive select) {
        Expression expression = select.expression();
        if (expression instanceof EntityPath entityPath) {
            BasicAttribute attribute = entityType.getAttribute(entityPath);
            if (expandReferencePath && attribute.isObject()) {
                return ((AssociationAttribute) attribute).getInstanceFactory();
            } else {
                return new InstanceFactories.AttributeFactoryImpl(attribute);
            }
        }
        return new InstanceFactories.PrimitiveFactoryImpl(
                expression, ExpressionTypeResolver.getExpressionType(expression, entityType));
    }

    public QueryContext newContext(QueryStructure structure) {
        return new QueryContext(structure, metamodel, expandReferencePath);
    }

    public Object construct(Arguments arguments) {
        return constructor.getInstance(arguments.iterator());
    }

}
