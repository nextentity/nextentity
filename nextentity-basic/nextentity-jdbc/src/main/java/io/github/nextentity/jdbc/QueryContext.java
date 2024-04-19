package io.github.nextentity.jdbc;

import io.github.nextentity.core.ExpressionTypeResolver;
import io.github.nextentity.core.api.expression.BaseExpression;
import io.github.nextentity.core.api.expression.QueryStructure;
import io.github.nextentity.core.api.expression.QueryStructure.From;
import io.github.nextentity.core.api.expression.QueryStructure.From.Entity;
import io.github.nextentity.core.meta.EntityType;
import io.github.nextentity.core.meta.Metamodel;
import io.github.nextentity.core.reflect.Arguments;
import io.github.nextentity.core.reflect.SchemaConstructor;
import io.github.nextentity.core.reflect.Timer;
import lombok.Data;

import java.util.List;

/**
 * @author HuangChengwei
 * @since 2024/4/20 下午12:03
 */
@Data
public class QueryContext {

    public static Timer timer = new Timer();

    protected final QueryStructure structure;
    protected final Metamodel metamodel;
    protected final EntityType entityType;
    protected final boolean expandReferencePath;
    protected final List<BaseExpression> selects ;
    protected final SchemaConstructor constructor;

    public QueryContext(QueryStructure structure, Metamodel metamodel, boolean expandObjectAttribute) {
        this.structure = structure;
        this.metamodel = metamodel;
        this.expandReferencePath = expandObjectAttribute;
        From from = structure.from();
        this.entityType = from instanceof Entity ? metamodel.getEntity(from.type()) : null;
        SelectedConstruct selectedConstruct = SelectedConstruct.of(entityType, structure.select(), expandObjectAttribute);
        this.constructor = selectedConstruct.constructor();
        this.selects = selectedConstruct.selects();
    }

    public QueryContext newContext(QueryStructure structure) {
        return new QueryContext(structure, metamodel, expandReferencePath);
    }

    public Object construct(Arguments arguments) {
        return constructor.construct(arguments);
    }

    public Class<?> getExpressionType(BaseExpression expression) {
        return ExpressionTypeResolver.getExpressionType(expression, entityType);
    }


}
