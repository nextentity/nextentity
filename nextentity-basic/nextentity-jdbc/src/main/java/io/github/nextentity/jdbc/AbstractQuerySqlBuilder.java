package io.github.nextentity.jdbc;

import io.github.nextentity.core.ExpressionTrees;
import io.github.nextentity.core.api.ExpressionTree.ExpressionNode;
import io.github.nextentity.core.api.LockModeType;
import io.github.nextentity.core.api.Operator;
import io.github.nextentity.core.api.Order;
import io.github.nextentity.core.api.SortOrder;
import io.github.nextentity.core.expression.From;
import io.github.nextentity.core.expression.From.Entity;
import io.github.nextentity.core.expression.From.FromSubQuery;
import io.github.nextentity.core.expression.Literal;
import io.github.nextentity.core.expression.Operation;
import io.github.nextentity.core.expression.PathChain;
import io.github.nextentity.core.expression.QueryStructure;
import io.github.nextentity.core.expression.Selection;
import io.github.nextentity.core.expression.Selection.EntitySelected;
import io.github.nextentity.core.expression.Selection.MultiSelected;
import io.github.nextentity.core.expression.Selection.ProjectionSelected;
import io.github.nextentity.core.expression.Selection.SingleSelected;
import io.github.nextentity.core.meta.AnyToOneAttribute;
import io.github.nextentity.core.meta.Attribute;
import io.github.nextentity.core.meta.BasicAttribute;
import io.github.nextentity.core.meta.EntityType;
import io.github.nextentity.core.meta.Metamodel;
import io.github.nextentity.core.meta.Projection;
import io.github.nextentity.core.meta.ProjectionAttribute;
import io.github.nextentity.core.meta.SubSelectType;
import io.github.nextentity.core.meta.Type;
import io.github.nextentity.core.util.Lists;
import io.github.nextentity.jdbc.JdbcQueryExecutor.PreparedSql;
import lombok.experimental.Accessors;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * @author HuangChengwei
 * @since 2024-04-11 8:28
 */
@SuppressWarnings("PatternVariableCanBeUsed")
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
    protected final Map<PathChain, Integer> joins = new LinkedHashMap<>();
    protected final QueryStructure queryStructure;

    protected final EntityType entity;
    protected final Metamodel mappers;
    protected final List<ExpressionNode> selectedExpressions = new ArrayList<>();
    protected final List<Attribute> selectedAttributes = new ArrayList<>();

    protected final String fromAlias;
    protected final int subIndex;
    protected final AtomicInteger selectIndex;

    public AbstractQuerySqlBuilder(StringBuilder sql,
                                   List<Object> args,
                                   QueryStructure queryStructure,
                                   Metamodel mappers,
                                   AtomicInteger selectIndex,
                                   int subIndex) {
        this.sql = sql;
        this.args = args;
        this.queryStructure = queryStructure;
        this.mappers = mappers;
        this.subIndex = subIndex;
        this.selectIndex = selectIndex;
        Class<?> type = queryStructure.from().type();
        String prefix;
        if (queryStructure.from() instanceof Entity) {
            prefix = sortAlias(type.getSimpleName());
            this.entity = mappers.getEntity(type);
        } else {
            prefix = "t";
            this.entity = null;
        }
        fromAlias = subIndex == 0 ? prefix + "_" : prefix + subIndex + "_";
    }

    public AbstractQuerySqlBuilder(QueryStructure queryStructure, Metamodel mappers) {
        this(new StringBuilder(), new ArrayList<>(), queryStructure, mappers, new AtomicInteger(), 0);
    }

    protected abstract String leftQuotedIdentifier();

    protected abstract String rightQuotedIdentifier();


    protected PreparedSql build() {
        doBuilder();
        return new PreparedSqlImpl(sql.toString(), args, selectedAttributes);
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
        appendLockModeType(queryStructure.lockType());
    }

    protected void initializeSelectedExpressions() {
        appendSelectedExpressions();
        appendFetchExpressions();
    }

    protected void appendSelectedExpressions() {
        Selection selected = queryStructure.select();
        if (selected instanceof SingleSelected) {
            SingleSelected singleSelected = (SingleSelected) selected;
            selectedExpressions.add(singleSelected.expression());
        } else if (selected instanceof MultiSelected) {
            MultiSelected multiSelected = (MultiSelected) selected;
            selectedExpressions.addAll(multiSelected.expressions());
        } else if (selected instanceof EntitySelected) {
            EntityType table = mappers
                    .getEntity(queryStructure.from().type());
            for (Attribute attribute : table.attributes()) {
                if (!(attribute instanceof BasicAttribute)) {
                    continue;
                }
                BasicAttribute column = (BasicAttribute) attribute;
                PathChain columns = ExpressionTrees.column(column.name());
                selectedExpressions.add(columns);
                selectedAttributes.add(attribute);
            }
        } else if (selected instanceof ProjectionSelected) {
            Projection projection = mappers
                    .getProjection(
                            queryStructure.from().type(),
                            queryStructure.select().resultType()
                    );
            for (ProjectionAttribute attr : projection.attributes()) {
                if (attr.entityAttribute() instanceof EntityType) {
                    continue;
                }
                PathChain columns = attr.entityAttribute();
                selectedExpressions.add(columns);
                selectedAttributes.add(attr);
            }
        } else {
            throw new IllegalStateException();
        }
    }

    protected void appendSelect() {
        initializeSelectedExpressions();
        sql.append(SELECT);
        if (queryStructure.select().distinct()) {
            sql.append(DISTINCT);
        }
        String join = NONE_DELIMITER;
        for (ExpressionNode expression : selectedExpressions) {
            sql.append(join);
            appendExpression(expression);
            appendSelectAlias(expression);
            join = DELIMITER;
        }
    }

    protected void appendSelectAlias(ExpressionNode expression) {
        if (selectIndex.get() != 0 || !(expression instanceof PathChain) || ((PathChain) expression).deep() != 1) {
            int index = selectIndex.getAndIncrement();
            String alias = Integer.toString(index, Character.MAX_RADIX);
            sql.append(" as _").append(alias);
        }
    }

    protected void appendFetchExpressions() {
        List<? extends PathChain> fetchClause = queryStructure.fetch();
        if (fetchClause != null && !fetchClause.isEmpty()) {
            PathChain[] array = fetchClause.stream()
                    .flatMap(it -> Lists.iterate(it, Objects::nonNull, PathChain::parent))
                    .distinct()
                    .toArray(PathChain[]::new);
            for (PathChain fetch : array) {
                Attribute attribute = getAttribute(fetch);
                if (!(attribute instanceof AnyToOneAttribute)) {
                    continue;
                }
                AnyToOneAttribute am = (AnyToOneAttribute) attribute;
                for (Attribute attr : am.attributes()) {
                    if (!(attr instanceof BasicAttribute)) {
                        continue;
                    }
                    PathChain column = fetch.get(attr.name());
                    selectedExpressions.add(column);
                    selectedAttributes.add(attr);
                }
            }
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
        From from = queryStructure.from();
        if (from instanceof Entity) {
            appendFromTable();
        } else if (from instanceof FromSubQuery) {
            FromSubQuery subQuery = (FromSubQuery) from;
            appendExpression(subQuery);
        }
        appendFromAlias();
    }

    protected void appendSubQuery(QueryStructure queryStructure) {
        sql.append('(');
        appendQueryStructure(queryStructure);
        sql.append(')');
    }

    protected abstract void appendQueryStructure(QueryStructure queryStructure);

    protected void appendFromTable() {
        appendTable(sql, entity);
        sql.append(" ");
    }

    protected StringBuilder appendFromAlias() {
        return appendFromAlias(sql);
    }

    protected StringBuilder appendFromAlias(StringBuilder sql) {
        return sql.append(fromAlias);
    }

    protected StringBuilder appendTableAlias(String table, Object index, StringBuilder sql) {
        StringBuilder append = appendBlank(sql).append(sortAlias(table));
        if (subIndex > 0) {
            sql.append(subIndex).append("_");
        }
        return append.append(index).append("_");
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
        ExpressionNode where = queryStructure.where();
        if (ExpressionTrees.isNullOrTrue(where)) {
            return;
        }
        sql.append(WHERE);
        appendPredicate(where);
    }

    protected void appendPredicate(ExpressionNode node) {
        if (node instanceof PathChain || node instanceof Literal) {
            node = ExpressionTrees.operate(node, Operator.EQ, ExpressionTrees.TRUE);
        }
        appendExpression(node);
    }


    protected void appendHaving() {
        ExpressionNode having = queryStructure.having();
        if (ExpressionTrees.isNullOrTrue(having)) {
            return;
        }
        sql.append(HAVING);
        appendPredicate(having);
    }

    protected void appendExpression(ExpressionNode expression) {
        if (expression instanceof Literal) {
            Literal constant = (Literal) expression;
            appendConstant(constant);
        } else if (expression instanceof PathChain) {
            PathChain column = (PathChain) expression;
            appendPaths(column);
        } else if (expression instanceof Operation) {
            Operation operation = (Operation) expression;
            appendOperation(operation);
        } else if (expression instanceof QueryStructure) {
            appendSubQuery(((QueryStructure) expression));
        } else {
            throw new UnsupportedOperationException("unknown type " + expression.getClass());
        }
    }

    protected void appendConstant(Literal constant) {
        Object value = constant.value();
        if (value instanceof Boolean) {
            Boolean b = (Boolean) value;
            appendBlank().append(b ? 1 : 0);
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
        ExpressionNode leftOperand = operation.firstOperand();
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
        ExpressionNode leftOperand = operation.firstOperand();
        Operator operator = operation.operator();
        if (operation.operands().size() <= 1) {
            appendBlank().append(0);
        } else {
            appendBlank();
            appendExpression(leftOperand);
            appendOperator(operator);
            List<? extends ExpressionNode> operands = operation.operands();
            boolean notSingleSubQuery = operands.size() != 2 || !(operands.get(1) instanceof QueryStructure);
            char join = notSingleSubQuery ? '(' : ' ';
            for (int i = 1; i < operands.size(); i++) {
                ExpressionNode expression = operands.get(i);
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
        ExpressionNode leftOperand = operation.firstOperand();
        appendOperator(operation.operator());
        List<? extends ExpressionNode> operands = operation.operands();
        boolean notSingleSubQuery = !(leftOperand instanceof QueryStructure) || operands.size() != 1;
        if (notSingleSubQuery) {
            sql.append('(');
        }
        appendExpression(leftOperand);
        for (int i = 1; i < operands.size(); i++) {
            ExpressionNode expression = operands.get(i);
            sql.append(',');
            appendExpression(expression);
        }
        if (notSingleSubQuery) {
            sql.append(")");
        }
    }

    protected void appendNotOperation(Operation operation) {
        ExpressionNode leftOperand = operation.firstOperand();
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
        List<? extends ExpressionNode> operands = operation.operands();
        for (int i = 1; i < operands.size(); i++) {
            ExpressionNode value = operands.get(i);
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
        ExpressionNode leftOperand = operation.firstOperand();
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
        ExpressionNode leftOperand = operation.firstOperand();
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
        List<? extends ExpressionNode> operands = operation.operands();
        for (int i = 1; i < operands.size(); i++) {
            ExpressionNode value = operands.get(i);
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

    protected void appendPaths(PathChain column) {
        appendBlank();
        int iMax = column.deep() - 1;
        if (iMax == -1)
            return;
        int i = 0;
        if (column.deep() == 1) {
            appendFromAlias().append(".");
        }
        Class<?> type = queryStructure.from().type();
        Attribute tail = column.toAttribute(mappers.getEntity(type));
        List<? extends Attribute> chain = tail.referencedAttributes();
        Attribute join = chain.get(0);

        for (String path : column) {
            EntityType info = mappers.getEntity(type);
            Attribute attribute = info.getAttribute(path);
            if (i++ == iMax) {
                if (attribute instanceof AnyToOneAttribute) {
                    AnyToOneAttribute ja = (AnyToOneAttribute) attribute;
                    sql.append(leftQuotedIdentifier()).append(ja.joinColumnName()).append(rightQuotedIdentifier());
                } else if (attribute instanceof BasicAttribute) {
                    BasicAttribute ba = (BasicAttribute) attribute;
                    sql.append(leftQuotedIdentifier()).append(ba.columnName()).append(rightQuotedIdentifier());
                } else {
                    throw new IllegalStateException();
                }
                return;
            } else {
                joins.putIfAbsent(join, joins.size());
                if (i == iMax) {
                    Integer index = joins.get(join);
                    appendTableAttribute(sql, attribute, index).append('.');
                }
            }
            type = attribute.javaType();
            join = join.get(path);
        }
    }

    protected void insertJoin(int sqlIndex) {
        StringBuilder sql = new StringBuilder();

        joins.forEach((k, v) -> {
            Attribute attribute = getAttribute(k);
            EntityType entityTypeInfo = mappers.getEntity(attribute.javaType());
            StringBuilder append = sql.append(" left join ");
            appendTable(append, entityTypeInfo);

            appendTableAttribute(sql, attribute, v);
            sql.append(ON);
            PathChain parent = k.parent();
            if (parent == null) {
                appendFromAlias(sql);
            } else {
                Integer parentIndex = joins.get(parent);
                Attribute parentAttribute = getAttribute(parent);
                appendTableAttribute(sql, parentAttribute, parentIndex);
            }
            if (attribute instanceof AnyToOneAttribute) {
                AnyToOneAttribute join = (AnyToOneAttribute) attribute;
                sql.append(".").append(join.joinColumnName()).append("=");
                appendTableAttribute(sql, attribute, v);
                String referenced = join.referencedColumnName();
                if (referenced.isEmpty()) {
                    referenced = ((BasicAttribute) entityTypeInfo.id()).columnName();
                }
                sql.append(".").append(referenced);
            } else {
                throw new IllegalStateException();
            }
        });
        this.sql.insert(sqlIndex, sql);

    }

    protected void appendTable(StringBuilder append, EntityType entityTypeInfo) {
        if (entityTypeInfo instanceof SubSelectType) {
            append.append('(').append(((SubSelectType) entityTypeInfo).subSelectSql()).append(')');
        } else {
            append.append(leftQuotedIdentifier()).append(entityTypeInfo.tableName()).append(rightQuotedIdentifier());
        }
    }

    Operator getOperator(ExpressionNode expression) {
        return expression instanceof Operation ? ((Operation) expression).operator() : null;
    }

    protected StringBuilder appendTableAttribute(StringBuilder sb, Attribute attribute, Integer index) {
        EntityType information = mappers.getEntity(attribute.javaType());
        String tableName = information.javaType().getSimpleName();
        return appendTableAlias(tableName, index, sb);
    }

    protected Attribute getAttribute(PathChain path) {
        Type schema = entity;
        for (String s : path) {
            if (schema instanceof EntityType) {
                EntityType ts = (EntityType) schema;
                schema = ts.getAttribute(s);
            } else {
                throw new IllegalStateException();
            }
        }
        return (Attribute) schema;
    }

    protected abstract void appendOffsetAndLimit();

    protected void appendGroupBy() {
        List<? extends ExpressionNode> groupBy = queryStructure.groupBy();
        if (groupBy != null && !groupBy.isEmpty()) {
            sql.append(" group by ");
            boolean first = true;
            for (ExpressionNode e : groupBy) {
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
        List<? extends Order<?>> orders = queryStructure.orderBy();
        if (orders != null && !orders.isEmpty()) {
            sql.append(ORDER_BY);
            String delimiter = "";
            for (Order<?> order : orders) {
                sql.append(delimiter);
                delimiter = ",";
                int selectIndex = selectedExpressions.indexOf(order.expression());
                if (selectIndex > 0) {
                    sql.append(selectIndex + 1);
                } else {
                    appendExpression(order.expression());
                }
                sql.append(" ").append(order.order() == SortOrder.DESC ? DESC : ASC);
            }

        }
    }

    @lombok.Data
    @Accessors(fluent = true)
    public static final class PreparedSqlImpl implements PreparedSql {
        private final String sql;
        private final List<?> args;
        private final List<Attribute> selected;
    }

}
