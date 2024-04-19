package io.github.nextentity.jdbc;

import io.github.nextentity.core.BasicExpressions;
import io.github.nextentity.core.SqlStatement;
import io.github.nextentity.core.api.LockModeType;
import io.github.nextentity.core.api.Operator;
import io.github.nextentity.core.api.Order;
import io.github.nextentity.core.api.SortOrder;
import io.github.nextentity.core.api.expression.BaseExpression;
import io.github.nextentity.core.api.expression.EntityPath;
import io.github.nextentity.core.api.expression.Literal;
import io.github.nextentity.core.api.expression.Operation;
import io.github.nextentity.core.api.expression.QueryStructure;
import io.github.nextentity.core.api.expression.QueryStructure.From;
import io.github.nextentity.core.api.expression.QueryStructure.From.Entity;
import io.github.nextentity.core.api.expression.QueryStructure.From.FromSubQuery;
import io.github.nextentity.core.meta.AssociationAttribute;
import io.github.nextentity.core.meta.BasicAttribute;
import io.github.nextentity.core.meta.EntitySchema;
import io.github.nextentity.core.meta.EntityType;
import io.github.nextentity.core.meta.SubSelectType;
import io.github.nextentity.core.reflect.schema.Schema;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
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
        if (from instanceof Entity) {
            prefix = sortAlias(from.type().getSimpleName());
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


    protected SqlStatement<?> build() {
        doBuilder();
        return new SqlStatement<>(sql.toString(), args);
    }

    protected void doBuilder() {
        appendSelect();
        appendFrom();
        int joinIndex = sql.length();
        appendWhere();
        appendGroupBy();
        appendOrderBy();
        appendHaving();
        insertJoin(joinIndex);
        appendOffsetAndLimit();
        appendLockModeType(context.getStructure().lockType());
    }

    protected void appendSelect() {
        sql.append(SELECT);
        if (context.getStructure().select().distinct()) {
            sql.append(DISTINCT);
        }
        String join = NONE_DELIMITER;
        for (BaseExpression expression : context.getSelects()) {
            sql.append(join);
            appendExpression(expression);
            appendSelectAlias(expression);
            join = DELIMITER;
        }
    }

    protected void appendSelectAlias(BaseExpression expression) {
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
        if (from instanceof Entity) {
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
    protected String sortAlias(String symbol) {
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
        BaseExpression where = context.getStructure().where();
        if (BasicExpressions.isNullOrTrue(where)) {
            return;
        }
        sql.append(WHERE);
        appendPredicate(where);
    }

    protected void appendPredicate(BaseExpression node) {
        if (node instanceof EntityPath || node instanceof Literal) {
            node = BasicExpressions.operate(node, Operator.EQ, BasicExpressions.TRUE);
        }
        appendExpression(node);
    }


    protected void appendHaving() {
        BaseExpression having = context.getStructure().having();
        if (BasicExpressions.isNullOrTrue(having)) {
            return;
        }
        sql.append(HAVING);
        appendPredicate(having);
    }

    protected void appendExpression(BaseExpression expression) {
        if (expression instanceof Literal) {
            appendConstant((Literal) expression);
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

    protected void appendConstant(Literal constant) {
        Object value = constant.value();
        if (value instanceof Boolean) {
            appendBlank().append((Boolean) value ? 1 : 0);
        } else {
            appendBlank().append('?');
            this.args.add(value);
        }
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
            case MOD:
            case GT:
            case EQ:
            case NE:
            case GE:
            case LT:
            case LE:
            case ADD:
            case SUBTRACT:
            case MULTIPLY:
            case DIVIDE: {
                appendBinaryOperation(operation);
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
            default:
                throw new UnsupportedOperationException("unknown operator " + operator);
        }
    }

    protected void appendNullAssertion(Operation operation) {
        appendFirstOperation(operation);
        appendBlank();
        appendOperator(operation.operator());
    }

    protected void appendFirstOperation(Operation operation) {
        BaseExpression leftOperand = operation.firstOperand();
        Operator operator = operation.operator();
        Operator operator0 = getOperator(leftOperand);
        appendBlank();
        if (operator0 != null && operator0.priority() > operator.priority()) {
            sql.append('(');
            appendExpression(leftOperand);
            sql.append(')');
        } else {
            appendExpression(leftOperand);
        }
    }

    protected void appendIn(Operation operation) {
        BaseExpression leftOperand = operation.firstOperand();
        Operator operator = operation.operator();
        if (operation.operands().size() <= 1) {
            appendBlank().append(0);
        } else {
            appendBlank();
            appendExpression(leftOperand);
            appendOperator(operator);
            List<? extends BaseExpression> operands = operation.operands();
            boolean notSingleSubQuery = operands.size() != 2 || !(operands.get(1) instanceof QueryStructure);
            char join = notSingleSubQuery ? '(' : ' ';
            for (int i = 1; i < operands.size(); i++) {
                BaseExpression expression = operands.get(i);
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
        BaseExpression leftOperand = operation.firstOperand();
        appendOperator(operation.operator());
        List<? extends BaseExpression> operands = operation.operands();
        boolean notSingleSubQuery = !(leftOperand instanceof QueryStructure) || operands.size() != 1;
        if (notSingleSubQuery) {
            sql.append('(');
        }
        appendExpression(leftOperand);
        for (int i = 1; i < operands.size(); i++) {
            BaseExpression expression = operands.get(i);
            sql.append(',');
            appendExpression(expression);
        }
        if (notSingleSubQuery) {
            sql.append(")");
        }
    }

    protected void appendNotOperation(Operation operation) {
        BaseExpression leftOperand = operation.firstOperand();
        Operator operator = operation.operator();
        Operator operator0 = getOperator(leftOperand);
        appendOperator(operator);
        sql.append(' ');
        if (operator0 != null && operator0.priority() > operator.priority()) {
            sql.append('(');
            appendExpression(leftOperand);
            sql.append(')');
        } else {
            appendExpression(leftOperand);
        }
    }

    protected void appendBinaryOperation(Operation operation) {
        appendFirstOperation(operation);
        Operator operator = operation.operator();
        List<? extends BaseExpression> operands = operation.operands();
        for (int i = 1; i < operands.size(); i++) {
            BaseExpression value = operands.get(i);
            appendOperator(operator);
            Operator operator1 = getOperator(value);
            if (operator1 != null && operator1.priority() >= operator.priority()) {
                sql.append('(');
                appendExpression(value);
                sql.append(')');
            } else {
                appendExpression(value);
            }
        }
    }

    protected void appendBetween(Operation operation) {
        BaseExpression leftOperand = operation.firstOperand();
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
        BaseExpression leftOperand = operation.firstOperand();
        Operator operator = operation.operator();
        Operator operator0 = getOperator(leftOperand);
        appendBlank();
        if (operator0 != null && operator0.priority() > operator.priority()) {
            sql.append('(');
            appendPredicate(leftOperand);
            sql.append(')');
        } else {
            appendPredicate(leftOperand);
        }
        List<? extends BaseExpression> operands = operation.operands();
        for (int i = 1; i < operands.size(); i++) {
            BaseExpression value = operands.get(i);
            appendOperator(operator);
            Operator operator1 = getOperator(value);
            if (operator1 != null && operator1.priority() >= operator.priority()) {
                sql.append('(');
                appendPredicate(value);
                sql.append(')');
            } else {
                appendPredicate(value);
            }
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
            return;
        int i = 0;
        if (column.deep() == 1) {
            appendFromAlias().append(".");
        }
        BasicAttribute tail = column.toAttribute(context.getEntityType());
        List<? extends BasicAttribute> chain = tail.attributePaths();
        BasicAttribute join = chain.get(0);

        for (String path : column) {
            EntitySchema info = context.getEntityType();
            BasicAttribute attribute = info.getAttribute(path);
            if (i++ == iMax) {
                if (attribute.isObject()) {
                    AssociationAttribute ja = (AssociationAttribute) attribute;
                    sql.append(leftQuotedIdentifier()).append(ja.joinColumnName()).append(rightQuotedIdentifier());
                } else {
                    sql.append(leftQuotedIdentifier()).append(attribute.columnName()).append(rightQuotedIdentifier());
                }
                return;
            } else {
                joins.putIfAbsent(join.path(), joins.size());
                if (i == iMax) {
                    Integer index = joins.get(join.path());
                    appendTableAlias(sql, index).append('.');
                }
            }
            join = ((AssociationAttribute) join).getAttribute(path);
        }
    }

    protected void insertJoin(int sqlIndex) {
        StringBuilder sql = new StringBuilder();

        joins.forEach((k, v) -> {
            BasicAttribute attribute = getAttribute(k);
            EntitySchema entityTypeInfo = context.getEntityType();
            StringBuilder append = sql.append(" left join ");
            appendTable(append, entityTypeInfo);

            appendTableAlias(sql, v);
            sql.append(ON);
            EntityPath parent = k.parent();
            if (parent == null) {
                appendFromAlias(sql);
            } else {
                Integer parentIndex = joins.get(parent);
                appendTableAlias(sql, parentIndex);
            }
            if (attribute.isObject()) {
                AssociationAttribute join = (AssociationAttribute) attribute;
                sql.append(".").append(join.joinColumnName()).append("=");
                appendTableAlias(sql, v);
                String referenced = join.referencedColumnName();
                if (referenced.isEmpty()) {
                    referenced = entityTypeInfo.id().columnName();
                }
                sql.append(".").append(referenced);
            } else {
                throw new IllegalStateException();
            }
        });
        this.sql.insert(sqlIndex, sql);

    }

    protected void appendTable(StringBuilder append, EntitySchema entityTypeInfo) {
        if (entityTypeInfo instanceof SubSelectType) {
            append.append('(').append(((SubSelectType) entityTypeInfo).subSelectSql()).append(')');
        } else {
            append.append(leftQuotedIdentifier()).append(entityTypeInfo.tableName()).append(rightQuotedIdentifier());
        }
    }

    Operator getOperator(BaseExpression expression) {
        return expression instanceof Operation ? ((Operation) expression).operator() : null;
    }

    protected StringBuilder appendTableAlias(StringBuilder sql, Integer index) {
        EntityType entityType = context.getEntityType();
        String tableName = entityType.type().getSimpleName();
        StringBuilder append = appendBlank(sql).append(sortAlias(tableName));
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
        List<? extends BaseExpression> groupBy = context.getStructure().groupBy();
        if (groupBy != null && !groupBy.isEmpty()) {
            sql.append(" group by ");
            boolean first = true;
            for (BaseExpression e : groupBy) {
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
                int selectIndex = context.getSelects().indexOf(order.expression());
                if (selectIndex > 0) {
                    sql.append(selectIndex + 1);
                } else {
                    appendExpression(order.expression());
                }
                sql.append(" ").append(order.order() == SortOrder.DESC ? DESC : ASC);
            }

        }
    }

}
