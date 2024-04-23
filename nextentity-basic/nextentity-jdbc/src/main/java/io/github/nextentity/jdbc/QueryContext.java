package io.github.nextentity.jdbc;

import io.github.nextentity.core.ExpressionTypeResolver;
import io.github.nextentity.core.api.expression.BaseExpression;
import io.github.nextentity.core.api.expression.EntityPath;
import io.github.nextentity.core.api.expression.QueryStructure;
import io.github.nextentity.core.api.expression.QueryStructure.From;
import io.github.nextentity.core.api.expression.QueryStructure.From.Entity;
import io.github.nextentity.core.api.expression.QueryStructure.Selected;
import io.github.nextentity.core.api.expression.QueryStructure.Selected.SelectEntity;
import io.github.nextentity.core.api.expression.QueryStructure.Selected.SelectProjection;
import io.github.nextentity.core.meta.EntityType;
import io.github.nextentity.core.meta.Metamodel;
import io.github.nextentity.core.reflect.Arguments;
import io.github.nextentity.core.reflect.ObjectFactory;
import io.github.nextentity.core.reflect.SelectedConstruct;
import lombok.Data;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.List;

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
    protected final List<BaseExpression> selects;
    protected final ObjectFactory constructor;

    public QueryContext(QueryStructure structure, Metamodel metamodel, boolean expandObjectAttribute) {
        this.structure = structure;
        this.metamodel = metamodel;
        this.expandReferencePath = expandObjectAttribute;
        From from = structure.from();
        this.entityType = from instanceof Entity ? metamodel.getEntity(from.type()) : null;
        SelectedConstruct selectedConstruct = getSelectedConstruct();
        this.constructor = selectedConstruct;
        this.selects = selectedConstruct.selects();
    }

    private @NotNull SelectedConstruct getSelectedConstruct() {
        Selected select = structure.select();
        if (select instanceof SelectEntity) {
            Collection<? extends EntityPath> fetch = ((SelectEntity) select).fetch();
            if (fetch == null || fetch.isEmpty()) {
                return entityType.constructor();
            }
        } else if (select instanceof SelectProjection) {
            return entityType.getProjection(select.type()).constructor();
        }
        return SelectedConstruct.of(entityType, structure.select(), expandReferencePath);
    }

    public QueryContext newContext(QueryStructure structure) {
        return new QueryContext(structure, metamodel, expandReferencePath);
    }

    public Object construct(Arguments arguments) {
        return constructor.get(arguments);
    }

    public Class<?> getExpressionType(BaseExpression expression) {
        return ExpressionTypeResolver.getExpressionType(expression, entityType);
    }


}
