package io.github.nextentity.core;

import io.github.nextentity.core.api.Operator;
import io.github.nextentity.core.api.expression.BaseExpression;
import io.github.nextentity.core.api.expression.EntityPath;
import io.github.nextentity.core.api.expression.Literal;
import io.github.nextentity.core.api.expression.Operation;
import io.github.nextentity.core.api.expression.QueryStructure;
import io.github.nextentity.core.meta.EntitySchema;
import io.github.nextentity.core.meta.EntityType;
import io.github.nextentity.core.reflect.PrimitiveTypes;
import io.github.nextentity.core.reflect.schema.Schema;
import io.github.nextentity.core.util.Lists;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.List;

/**
 * @author HuangChengwei
 * @since 2024-03-26 9:01
 */
public class ExpressionTypeResolver {

    private static final List<Class<? extends Number>> NUMBER_TYPES = Lists.of(
            Byte.class,
            Short.class,
            Integer.class,
            Long.class,
            BigInteger.class,
            Float.class,
            Double.class,
            BigDecimal.class
    );


    public static Class<?> getExpressionType(BaseExpression expression, EntityType entityType) {
        if (expression instanceof EntityPath) {
            return getColumnType((EntityPath) expression, entityType);
        } else if (expression instanceof Literal) {
            return getLiteralType((Literal) expression);
        } else if (expression instanceof Operation) {
            return getOperationType((Operation) expression, entityType);
        } else if (expression instanceof QueryStructure) {
            return getSubQueryType((QueryStructure) expression);
        }
        return Object.class;
    }

    private static Class<?> getSubQueryType(QueryStructure subQuery) {
        return subQuery.from().type();
    }

    public static Class<?> getOperationType(Operation expression, EntityType entityType) {
        Operator operator = expression.operator();
        // noinspection EnhancedSwitchMigration
        switch (operator) {
            case NOT:
            case AND:
            case OR:
            case GT:
            case EQ:
            case NE:
            case GE:
            case LT:
            case LE:
            case LIKE:
            case IS_NULL:
            case IS_NOT_NULL:
            case IN:
            case BETWEEN:
                return Boolean.class;
            case LOWER:
            case UPPER:
            case SUBSTRING:
            case TRIM:
                return String.class;
            case LENGTH:
            case COUNT:
                return Long.class;
            case ADD:
            case SUBTRACT:
            case MULTIPLY:
            case MOD:
            case SUM:
                return getNumberType(expression, entityType);
            case DIVIDE:
            case AVG:
                return Double.class;
            case NULLIF:
            case IF_NULL:
            case MIN:
            case MAX:
                return getFirstOperandType(expression, entityType);
        }
        return Object.class;
    }

    private static Class<?> getFirstOperandType(Operation expression, EntityType entityType) {
        if (!expression.operands().isEmpty()) {
            return getExpressionType(expression.operands().get(0), entityType);
        }
        return Object.class;
    }

    private static Class<?> getNumberType(Operation expression, EntityType entityType) {
        int index = -1;
        for (BaseExpression operand : expression.operands()) {
            Class<?> type = getExpressionType(operand, entityType);
            if (type.isPrimitive()) {
                type = PrimitiveTypes.getWrapper(type);
            }
            int i = NUMBER_TYPES.indexOf(type);
            if (i < 0 || i == NUMBER_TYPES.size() - 1) {
                index = i;
                break;
            }
            index = Math.max(index, i);
        }
        if (index >= 0 && index < NUMBER_TYPES.size()) {
            return NUMBER_TYPES.get(index);
        }
        return Object.class;
    }

    public static Class<?> getLiteralType(Literal expression) {
        return expression.value().getClass();
    }

    public static Class<?> getColumnType(EntityPath column, EntityType entityType) {
        Schema t = entityType;
        for (String s : column) {
            t = ((EntitySchema) t).getAttribute(s);
        }
        return t.type();
    }

}
