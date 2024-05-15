package io.github.nextentity.jdbc;

import io.github.nextentity.api.Expression;
import io.github.nextentity.api.model.LockModeType;
import io.github.nextentity.api.model.Order;
import io.github.nextentity.api.SortOrder;
import io.github.nextentity.core.expression.EntityPath;
import io.github.nextentity.core.expression.Literal;
import io.github.nextentity.core.expression.Operation;
import io.github.nextentity.core.expression.Operator;
import io.github.nextentity.core.expression.QueryStructure;
import io.github.nextentity.core.expression.QueryStructure.From;
import io.github.nextentity.core.expression.QueryStructure.From.FromEntity;
import io.github.nextentity.core.expression.QueryStructure.From.FromSubQuery;
import io.github.nextentity.core.expression.impl.ExpressionImpls;
import io.github.nextentity.core.meta.AssociationAttribute;
import io.github.nextentity.core.meta.BasicAttribute;
import io.github.nextentity.core.meta.EntitySchema;
import io.github.nextentity.core.meta.EntityType;
import io.github.nextentity.core.meta.SubSelectType;
import io.github.nextentity.core.reflect.schema.InstanceFactory;
import io.github.nextentity.core.reflect.schema.Schema;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * @author HuangChengwei
 * @since 2024-04-11 8:28
 */
abstract class AbstractQuerySqlBuilder {

    protected static final String NONE_DELIMITER = "";
    protected static final String DELIMITER = ",";
    protected static final String FOR_SHARE = " for share";
    protected static final String FOR_UPDATE = " for update";
    protected static final String FOR_UPDATE_NOWAIT = " for update nowait";
    protected static final String SELECT = "select ";
    protected static final String DISTINCT = "distinct ";
    protected static final String FROM = "from ";
    protected static final String WHERE = " where ";
    protected static final String HAVING = " having ";
    protected static final String ORDER_BY = " order by ";
    protected static final String DESC = "desc";
    protected static final String ASC = "asc";
    protected static final String LEFT_JOIN = " left join ";
    protected static final String ON = " on ";

    protected final StringBuilder sql;
    protected final List<Object> args;
    protected final Map<EntityPath, Integer> joins = new LinkedHashMap<>();

    protected final QueryContext context;

    protected final String fromAlias;
    protected final int subIndex;
    protected final AtomicInteger selectIndex;

    protected AbstractQuerySqlBuilder(StringBuilder sql,
                                      List<Object> args,
                                      QueryContext context,
                                      AtomicInteger selectIndex,
                                      int subIndex) {
        this.sql = sql;
        this.args = args;
        this.subIndex = subIndex;
        this.selectIndex = selectIndex;
        this.context = context;
        String prefix;
        From from = context.getStructure().from();
        if (from instanceof FromEntity) {
            prefix = shortAlias(from.type().getSimpleName());
        } else {
            prefix = "t";
        }
        fromAlias = subIndex == 0 ? prefix + "_" : prefix + subIndex + "_";
    }

    public AbstractQuerySqlBuilder(QueryContext context) {
        this(new StringBuilder(), new ArrayList<>(), context, new AtomicInteger(), 0);
    }

    protected abstract String leftQuotedIdentifier();

    protected abstract String rightQuotedIdentifier();


    protected QuerySqlStatement build() {
        doBuilder();
        return new QuerySqlStatement(sql.toString(), args);
    }

    protected void doBuilder() {
        initJoinColumnIndex();
        appendSelect();
        appendFrom();
        appendJoin();
        appendWhere();
        appendGroupBy();
        appendOrderBy();
        appendHaving();
        appendOffsetAndLimit();
        appendLockModeType(context.getStructure().lockType());
    }

    protected void appendSelect() {
        sql.append(SELECT);
        if (context.getStructure().select().distinct()) {
            sql.append(DISTINCT);
        }
        String join = NONE_DELIMITER;
        for (InstanceFactory.PrimitiveFactory factory : context.getConstructor().primitives()) {
            Expression expression = factory.expression();
            sql.append(join);
            appendExpression(expression);
            appendSelectAlias(expression);
            join = DELIMITER;
        }
    }

    protected void appendSelectAlias(Expression expression) {
        if (selectIndex.get() != 0 || !(expression instanceof EntityPath) || ((EntityPath) expression).deep() != 1) {
            int index = selectIndex.getAndIncrement();
            String alias = Integer.toString(index, Character.MAX_RADIX);
            sql.append(" as _").append(alias);
        }
    }

    protected void appendLockModeType(LockModeType lockModeType) {
        if (lockModeType == LockModeType.PESSIMISTIC_READ) {
            sql.append(FOR_SHARE);
        } else if (lockModeType == LockModeType.PESSIMISTIC_WRITE) {
            sql.append(FOR_UPDATE);
        } else if (lockModeType == LockModeType.PESSIMISTIC_FORCE_INCREMENT) {
            sql.append(FOR_UPDATE_NOWAIT);
        }
    }

    protected void appendFrom() {
        appendBlank().append(FROM);
        From from = context.getStructure().from();
        if (from instanceof FromEntity) {
            appendFromTable();
        } else if (from instanceof FromSubQuery) {
            appendExpression((FromSubQuery) from);
        }
        appendFromAlias();
    }

    protected void appendSubQuery(QueryStructure queryStructure) {
        sql.append('(');
        QueryContext ctx = context.newContext(queryStructure);
        appendQueryStructure(ctx);
        sql.append(')');
    }

    protected abstract void appendQueryStructure(QueryContext subContext);

    protected void appendFromTable() {
        appendTable(sql, context.getEntityType());
        sql.append(" ");
    }

    protected StringBuilder appendFromAlias() {
        return appendFromAlias(sql);
    }

    protected StringBuilder appendFromAlias(StringBuilder sql) {
        return sql.append(fromAlias);
    }

    @NotNull
    protected String shortAlias(String symbol) {
        return symbol.toLowerCase().substring(0, 1);
    }

    protected StringBuilder appendBlank() {
        return appendBlank(sql);
    }

    protected StringBuilder appendBlank(StringBuilder sql) {
        // noinspection SizeReplaceableByIsEmpty
        return sql.length() == 0 || " (,+-*/%=><".indexOf(sql.charAt(sql.length() - 1)) >= 0 ? sql : sql.append(' ');
    }

    protected void appendWhere() {
        Expression where = context.getStructure().where();
        if (ExpressionImpls.isNullOrTrue(where)) {
            return;
        }
        sql.append(WHERE);
        appendPredicate(where);
    }

    protected void appendPredicate(Expression node) {
        if (node instanceof EntityPath || node instanceof Literal) {
            node = ExpressionImpls.operate(node, Operator.EQ, ExpressionImpls.TRUE);
        }
        appendExpression(node);
    }

    protected void appendHaving() {
        Expression having = context.getStructure().having();
        if (ExpressionImpls.isNullOrTrue(having)) {
            return;
        }
        sql.append(HAVING);
        appendPredicate(having);
    }

    protected void appendExpression(Expression expression) {
        if (expression instanceof Literal) {
            appendLiteral((Literal) expression);
        } else if (expression instanceof EntityPath) {
            appendPaths((EntityPath) expression);
        } else if (expression instanceof Operation) {
            appendOperation((Operation) expression);
        } else if (expression instanceof QueryStructure) {
            appendSubQuery(((QueryStructure) expression));
        } else {
            throw new UnsupportedOperationException("unknown type " + expression.getClass());
        }
    }

    protected void appendLiteral(Literal literal) {
        appendBlank().append('?');
        this.args.add(literal.value());
    }

    protected void appendOperation(Operation operation) {
        Operator operator = operation.operator();
        switch (operator) {
            case NOT: {
                appendNotOperation(operation);
                break;
            }
            case AND:
            case OR: {
                appendLogicalOperation(operation);
                break;
            }
            case LIKE:
            case GT:
            case EQ:
            case NE:
            case GE:
            case LT:
            case LE: {
                appendBinaryOperation(operation);
                break;
            }
            case ADD:
            case SUBTRACT:
            case MULTIPLY:
            case DIVIDE:
            case MOD: {
                appendMultiOperation(operation);
                break;
            }
            case LENGTH:
            case LOWER:
            case UPPER:
            case SUBSTRING:
            case TRIM:
            case NULLIF:
            case IF_NULL:
            case MIN:
            case MAX:
            case COUNT:
            case AVG:
            case SUM: {
                appendFunctionOperation(operation);
                break;
            }
            case IN: {
                appendIn(operation);
                break;
            }
            case BETWEEN: {
                appendBetween(operation);
                break;
            }
            case IS_NULL:
            case IS_NOT_NULL: {
                appendNullAssertion(operation);
                break;
            }
            case DISTINCT:
                appendPrepositionOperation(operation);
                break;
            default:
                throw new UnsupportedOperationException("unknown operator " + operator);
        }
    }

    protected void appendNullAssertion(Operation operation) {
        appendFirstOperation(operation);
        appendBlank();
        appendOperator(operation.operator());
    }

    protected void appendPrepositionOperation(Operation operation) {
        Expression operand = operation.firstOperand();
        Operator operator = operation.operator();
        appendOperator(operator);
        appendExpressionPriority(operand, operator);
    }

    protected void appendFirstOperation(Operation operation) {
        Expression leftOperand = operation.firstOperand();
        Operator operator = operation.operator();
        appendExpressionPriority(leftOperand, operator);
    }

    protected void appendIn(Operation operation) {
        Expression leftOperand = operation.firstOperand();
        Operator operator = operation.operator();
        if (operation.operands().size() <= 1) {
            appendBlank().append(0);
        } else {
            appendBlank();
            appendExpression(leftOperand);
            appendOperator(operator);
            List<? extends Expression> operands = operation.operands();
            boolean notSingleSubQuery = operands.size() != 2 || !(operands.get(1) instanceof QueryStructure);
            char join = notSingleSubQuery ? '(' : ' ';
            for (int i = 1; i < operands.size(); i++) {
                Expression expression = operands.get(i);
                sql.append(join);
                appendExpression(expression);
                join = ',';
            }
            if (notSingleSubQuery) {
                sql.append(")");
            }
        }
    }

    protected void appendFunctionOperation(Operation operation) {
        Expression leftOperand = operation.firstOperand();
        appendOperator(operation.operator());
        List<? extends Expression> operands = operation.operands();
        boolean notSingleSubQuery = !(leftOperand instanceof QueryStructure) || operands.size() != 1;
        if (notSingleSubQuery) {
            sql.append('(');
        }
        appendExpression(leftOperand);
        for (int i = 1; i < operands.size(); i++) {
            Expression expression = operands.get(i);
            sql.append(',');
            appendExpression(expression);
        }
        if (notSingleSubQuery) {
            sql.append(")");
        }
    }

    protected void appendNotOperation(Operation operation) {
        Expression leftOperand = operation.firstOperand();
        Operator not = operation.operator();
        appendBlank();
        appendOperator(not);
        appendPredicatePriority(leftOperand, not);
    }

    protected void appendBinaryOperation(Operation operation) {
        Expression leftOperand = operation.firstOperand();
        Operator operator = operation.operator();
        Expression rightOperand = operation.secondOperand();
        appendBinaryOperation(leftOperand, operator, rightOperand);
    }

    protected void appendBinaryOperation(Expression leftOperand,
                                         Operator operator,
                                         Expression rightOperand) {
        appendExpressionPriority(leftOperand, operator);
        appendOperator(operator);
        appendExpressionPriority(rightOperand, operator);
    }

    protected void appendMultiOperation(Operation operation) {
        appendFirstOperation(operation);
        Operator operator = operation.operator();
        List<? extends Expression> operands = operation.operands();
        for (int i = 1; i < operands.size(); i++) {
            Expression value = operands.get(i);
            appendOperator(operator);
            appendExpressionPriority(value, operator);
        }
    }

    protected void appendBetween(Operation operation) {
        Expression leftOperand = operation.firstOperand();
        Operator operator = operation.operator();
        appendBlank();
        appendExpression(leftOperand);
        appendOperator(operator);
        appendBlank();
        appendExpression(operation.secondOperand());
        appendOperator(Operator.AND);
        appendBlank();
        appendExpression(operation.thirdOperand());
    }

    protected void appendLogicalOperation(Operation operation) {
        Expression leftOperand = operation.firstOperand();
        Operator operator = operation.operator();
        appendBlank();
        appendPredicatePriority(leftOperand, operator);
        List<? extends Expression> operands = operation.operands();
        for (int i = 1; i < operands.size(); i++) {
            Expression value = operands.get(i);
            appendOperator(operator);
            appendPredicatePriority(value, operator);
        }
    }

    private void appendPredicatePriority(Expression expression, Operator operator) {
        Operator append = getOperator(expression);
        if (append != null && append.priority() > operator.priority()) {
            sql.append('(');
            appendPredicate(expression);
            sql.append(')');
        } else {
            appendPredicate(expression);
        }
    }

    private void appendExpressionPriority(Expression value, Operator operator) {
        Operator next = getOperator(value);
        if (next != null && next.priority() > operator.priority()) {
            sql.append('(');
            appendExpression(value);
            sql.append(')');
        } else {
            appendExpression(value);
        }
    }

    protected void appendOperator(Operator jdbcOperator) {
        String sign = jdbcOperator.sign();
        appendOperator(sign);
    }

    protected void appendOperator(String sign) {
        if (Character.isLetter(sign.charAt(0))) {
            appendBlank();
        }
        sql.append(sign);
    }

    protected void appendPaths(EntityPath column) {
        appendBlank();
        int iMax = column.deep() - 1;
        if (iMax == -1)
            throw new IllegalStateException();
        BasicAttribute attribute = column.toAttribute(context.getEntityType());
        if (column.deep() == 1) {
            appendFromAlias().append(".");
        } else {
            BasicAttribute join = (BasicAttribute) attribute.declareBy();
            Integer index = joins.get(join.path());
            appendTableAlias(index).append('.');
        }
        sql.append(leftQuotedIdentifier()).append(attribute.columnName()).append(rightQuotedIdentifier());
    }

    protected void appendJoin() {
        for (Entry<EntityPath, Integer> entry : joins.entrySet()) {
            EntityPath k = entry.getKey();
            Integer v = entry.getValue();
            BasicAttribute attribute = getAttribute(k);
            EntitySchema entityTypeInfo = (EntitySchema) context.getEntityType().getAttribute(k);
            StringBuilder append = sql.append(LEFT_JOIN);
            appendTable(append, entityTypeInfo);

            appendTableAlias(v);
            sql.append(ON);
            EntitySchema declared = attribute.declareBy();
            if (declared.isAttribute()) {
                EntityPath parent = ((BasicAttribute) declared).path();
                Integer parentIndex = joins.get(parent);
                appendTableAlias(parentIndex);
            } else {
                appendFromAlias(sql);
            }
            if (attribute.isObject()) {
                AssociationAttribute join = (AssociationAttribute) attribute;
                sql.append(".").append(join.columnName()).append("=");
                appendTableAlias(v);
                String referenced = join.referencedColumnName();
                if (referenced.isEmpty()) {
                    referenced = entityTypeInfo.id().columnName();
                }
                sql.append(".").append(referenced);
            } else {
                throw new IllegalStateException();
            }
        }

    }

    private void initJoinColumnIndex() {
        QueryStructure structure = context.getStructure();
        addJoinPrimitive(context.getConstructor().primitives());
        addJoin(structure.where());
        addJoin(structure.groupBy());
        for (Order<?> order : structure.orderBy()) {
            addJoin(order.expression());
        }
        addJoin(structure.having());
    }

    private void addJoin(Expression select) {
        if (select instanceof EntityPath) {
            EntityType entityType = context.getEntityType();
            BasicAttribute attribute = entityType.getAttribute((EntityPath) select);
            for (BasicAttribute join : attribute.attributePaths()) {
                if (join.isObject()) {
                    joins.putIfAbsent(join.path(), joins.size());
                }
            }
        } else if (select instanceof Operation) {
            addJoin(((Operation) select).operands());
        }
    }

    private void addJoinPrimitive(List<? extends InstanceFactory.PrimitiveFactory> operands) {
        if (operands != null && !operands.isEmpty()) {
            for (InstanceFactory.PrimitiveFactory operand : operands) {
                addJoin(operand.expression());
            }
        }
    }

    private void addJoin(List<? extends Expression> operands) {
        if (operands != null && !operands.isEmpty()) {
            for (Expression operand : operands) {
                addJoin(operand);
            }
        }
    }

    protected void appendTable(StringBuilder append, EntitySchema entityTypeInfo) {
        if (entityTypeInfo instanceof SubSelectType) {
            append.append('(').append(((SubSelectType) entityTypeInfo).subSelectSql()).append(')');
        } else {
            append.append(leftQuotedIdentifier()).append(entityTypeInfo.tableName()).append(rightQuotedIdentifier());
        }
    }

    Operator getOperator(Expression expression) {
        return expression instanceof Operation ? ((Operation) expression).operator() : null;
    }

    protected StringBuilder appendTableAlias(Integer index) {
        StringBuilder sql = this.sql;
        EntityType entityType = context.getEntityType();
        String tableName = entityType.type().getSimpleName();
        StringBuilder append = appendBlank(sql).append(shortAlias(tableName));
        if (subIndex > 0) {
            sql.append(subIndex).append("_");
        }
        return append.append(index).append("_");
    }

    protected BasicAttribute getAttribute(EntityPath path) {
        Schema schema = context.getEntityType();
        for (String s : path) {
            if (schema instanceof EntitySchema) {
                schema = ((EntitySchema) schema).getAttribute(s);
            } else {
                throw new IllegalStateException();
            }
        }
        return (BasicAttribute) schema;
    }

    protected abstract void appendOffsetAndLimit();

    protected void appendGroupBy() {
        List<? extends Expression> groupBy = context.getStructure().groupBy();
        if (groupBy != null && !groupBy.isEmpty()) {
            sql.append(" group by ");
            boolean first = true;
            for (Expression e : groupBy) {
                if (first) {
                    first = false;
                } else {
                    sql.append(",");
                }
                appendExpression(e);
            }
        }
    }

    protected void appendOrderBy() {
        List<? extends Order<?>> orders = context.getStructure().orderBy();
        if (orders != null && !orders.isEmpty()) {
            sql.append(ORDER_BY);
            String delimiter = "";
            for (Order<?> order : orders) {
                sql.append(delimiter);
                delimiter = ",";
                int selectIndex = getSelectIndex(order);
                if (selectIndex > 0) {
                    sql.append(selectIndex + 1);
                } else {
                    appendExpression(order.expression());
                }
                sql.append(" ").append(order.order() == SortOrder.DESC ? DESC : ASC);
            }

        }
    }

    private int getSelectIndex(Order<?> order) {
        List<? extends InstanceFactory.PrimitiveFactory> primitives = context.getConstructor().primitives();
        for (int i = 0; i < primitives.size(); i++) {
            InstanceFactory.PrimitiveFactory primitive = primitives.get(i);
            if (primitive.expression().equals(order.expression())) {
                return i;
            }
        }
        return -1;
    }

}
