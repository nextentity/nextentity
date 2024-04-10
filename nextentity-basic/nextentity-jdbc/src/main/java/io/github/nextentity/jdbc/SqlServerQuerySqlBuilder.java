package io.github.nextentity.jdbc;

import io.github.nextentity.core.ExpressionTrees;
import io.github.nextentity.core.api.ExpressionTree.Column;
import io.github.nextentity.core.api.ExpressionTree.ExpressionNode;
import io.github.nextentity.core.api.ExpressionTree.Literal;
import io.github.nextentity.core.api.ExpressionTree.Operation;
import io.github.nextentity.core.api.ExpressionTree.QueryStructure;
import io.github.nextentity.core.api.ExpressionTree.QueryStructure.From;
import io.github.nextentity.core.api.ExpressionTree.QueryStructure.From.Entity;
import io.github.nextentity.core.api.ExpressionTree.QueryStructure.From.FromSubQuery;
import io.github.nextentity.core.api.ExpressionTree.QueryStructure.Order;
import io.github.nextentity.core.api.ExpressionTree.QueryStructure.Selection;
import io.github.nextentity.core.api.ExpressionTree.QueryStructure.Selection.EntitySelected;
import io.github.nextentity.core.api.ExpressionTree.QueryStructure.Selection.MultiSelected;
import io.github.nextentity.core.api.ExpressionTree.QueryStructure.Selection.ProjectionSelected;
import io.github.nextentity.core.api.ExpressionTree.QueryStructure.Selection.SingleSelected;
import io.github.nextentity.core.api.LockModeType;
import io.github.nextentity.core.api.Operator;
import io.github.nextentity.core.api.SortOrder;
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
import io.github.nextentity.jdbc.JdbcQueryExecutor.QuerySqlBuilder;
import lombok.experimental.Accessors;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicInteger;

public class SqlServerQuerySqlBuilder implements QuerySqlBuilder {

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
    protected static final char LW = '[';
    protected static final char RW = ']';

    @Override
    public PreparedSql build(QueryStructure structure, Metamodel metamodel) {
        return new Builder(structure, metamodel).build();
    }

    @SuppressWarnings("PatternVariableCanBeUsed")
    static class Builder {

        protected final StringBuilder sql;
        protected final List<Object> args;
        protected final Map<Column, Integer> joins = new LinkedHashMap<>();
        protected final QueryStructure queryStructure;

        protected final EntityType entity;
        protected final Metamodel mappers;
        protected final List<ExpressionNode> selectedExpressions = new ArrayList<>();
        protected final List<Attribute> selectedAttributes = new ArrayList<>();

        protected final String fromAlias;
        protected final int subIndex;
        protected final AtomicInteger selectIndex;

        public Builder(StringBuilder sql,
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

        public Builder(QueryStructure queryStructure, Metamodel mappers) {
            this(new StringBuilder(), new ArrayList<>(), queryStructure, mappers, new AtomicInteger(), 0);
        }

        protected PreparedSql build() {
            doBuilder();
            return new PreparedSqlImpl(sql.toString(), args, selectedAttributes);
        }

        private void doBuilder() {
            boolean distinctAndHasLimit = distinctAndHasLimit();
            if (distinctAndHasLimit) {
                sql.append(SELECT).append("* from (");
            }
            appendSelect();
            appendFrom();
            int joinIndex = sql.length();
            appendWhere();
            appendGroupBy();
            appendOrderBy();
            appendHaving();
            insertJoin(joinIndex);
            if (distinctAndHasLimit) {
                sql.append(") ").append(fromAlias).append("dst");
            }
            appendOffsetAndLimit(distinctAndHasLimit || noOrderBy());
            appendLockModeType(queryStructure.lockType());
        }

        private boolean distinctAndHasLimit() {
            return queryStructure.select().distinct() &&
                   (unwrap(queryStructure.offset()) > 0 || unwrap(queryStructure.limit()) >= 0);
        }

        private void initializeSelectedExpressions() {
            appendSelectedExpressions();
            appendFetchExpressions();
        }

        private void appendSelectedExpressions() {
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
                    Column columns = ExpressionTrees.column(column.name());
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
                    Column columns = attr.entityAttribute().column();
                    selectedExpressions.add(columns);
                    selectedAttributes.add(attr);
                }
            } else {
                throw new IllegalStateException();
            }
        }

        private static int unwrap(Integer offset) {
            return offset == null ? -1 : offset;
        }

        private void appendSelect() {
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

        private void appendSelectAlias(ExpressionNode expression) {
            if (selectIndex.get() != 0 || !(expression instanceof Column) || ((Column) expression).size() != 1) {
                int index = selectIndex.getAndIncrement();
                String alias = Integer.toString(index, Character.MAX_RADIX);
                sql.append(" as _").append(alias);
            }
        }

        protected void appendFetchExpressions() {
            List<? extends Column> fetchClause = queryStructure.fetch();
            if (fetchClause != null && !fetchClause.isEmpty()) {
                Column[] array = fetchClause.stream()
                        .flatMap(it -> Lists.iterate(it, Objects::nonNull, Column::parent))
                        .distinct()
                        .toArray(Column[]::new);
                for (Column fetch : array) {
                    Attribute attribute = getAttribute(fetch);
                    if (!(attribute instanceof AnyToOneAttribute)) {
                        continue;
                    }
                    AnyToOneAttribute am = (AnyToOneAttribute) attribute;
                    for (Attribute attr : am.attributes()) {
                        if (!(attr instanceof BasicAttribute)) {
                            continue;
                        }
                        Column column = fetch.get(attr.name());
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

        private void appendFrom() {
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

        private void appendSubQuery(QueryStructure queryStructure) {
            sql.append('(');
            new Builder(sql, args, queryStructure, mappers, selectIndex, subIndex + 1).doBuilder();
            sql.append(')');
        }

        private void appendFromTable() {
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
        private static String sortAlias(String symbol) {
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

        private void appendPredicate(ExpressionNode node) {
            appendPredicate(args, node);
        }

        private void appendPredicate(List<Object> args, ExpressionNode node) {
            if (node instanceof Column || node instanceof Literal) {
                node = ExpressionTrees.operate(node, Operator.EQ, ExpressionTrees.TRUE);
            }
            appendExpression(args, node);
        }


        protected void appendHaving() {
            ExpressionNode having = queryStructure.having();
            if (ExpressionTrees.isNullOrTrue(having)) {
                return;
            }
            sql.append(HAVING);
            appendPredicate(having);
        }

        protected void appendExpression(ExpressionNode expr) {
            appendExpression(args, expr);
        }

        private void appendExpression(List<Object> args, ExpressionNode expression) {
            if (expression instanceof Literal) {
                Literal constant = (Literal) expression;
                appendConstant(args, constant);
            } else if (expression instanceof Column) {
                Column column = (Column) expression;
                appendPaths(column);
            } else if (expression instanceof Operation) {
                Operation operation = (Operation) expression;
                appendOperation(args, operation);
            } else if (expression instanceof QueryStructure) {
                appendSubQuery(((QueryStructure) expression));
            } else {
                throw new UnsupportedOperationException("unknown type " + expression.getClass());
            }
        }

        private void appendConstant(List<Object> args, Literal constant) {
            Object value = constant.value();
            if (value instanceof Boolean) {
                Boolean b = (Boolean) value;
                appendBlank().append(b ? 1 : 0);
            } else {
                appendBlank().append('?');
                args.add(value);
            }
        }

        private void appendOperation(List<Object> args, Operation operation) {
            Operator operator = operation.operator();
            ExpressionNode leftOperand = operation.firstOperand();
            Operator operator0 = getOperator(leftOperand);
            String sign = operator.sign();
            switch (operator) {
                case NOT: {
                    appendOperator(operator);
                    sql.append(' ');
                    if (operator0 != null && operator0.priority() > operator.priority()) {
                        sql.append('(');
                        appendExpression(args, leftOperand);
                        sql.append(')');
                    } else {
                        appendExpression(args, leftOperand);
                    }
                    break;
                }
                case AND:
                case OR: {
                    appendBlank();
                    if (operator0 != null && operator0.priority() > operator.priority()) {
                        sql.append('(');
                        appendPredicate(args, leftOperand);
                        sql.append(')');
                    } else {
                        appendPredicate(args, leftOperand);
                    }
                    List<? extends ExpressionNode> operands = operation.operands();
                    for (int i = 1; i < operands.size(); i++) {
                        ExpressionNode value = operands.get(i);
                        appendOperator(operator);
                        Operator operator1 = getOperator(value);
                        if (operator1 != null && operator1.priority() >= operator.priority()) {
                            sql.append('(');
                            appendPredicate(args, value);
                            sql.append(')');
                        } else {
                            appendPredicate(args, value);
                        }
                    }
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
                    appendBlank();
                    if (operator0 != null && operator0.priority() > operator.priority()) {
                        sql.append('(');
                        appendExpression(args, leftOperand);
                        sql.append(')');
                    } else {
                        appendExpression(args, leftOperand);
                    }
                    List<? extends ExpressionNode> operands = operation.operands();
                    for (int i = 1; i < operands.size(); i++) {
                        ExpressionNode value = operands.get(i);
                        appendOperator(operator);
                        Operator operator1 = getOperator(value);
                        if (operator1 != null && operator1.priority() >= operator.priority()) {
                            sql.append('(');
                            appendExpression(args, value);
                            sql.append(')');
                        } else {
                            appendExpression(args, value);
                        }
                    }
                    break;
                }
                case LENGTH:
                    sign = "len";
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
                    appendOperator(sign);
                    List<? extends ExpressionNode> operands = operation.operands();
                    boolean notSingleSubQuery = !(leftOperand instanceof QueryStructure) || operands.size() != 1;
                    if (notSingleSubQuery) {
                        sql.append('(');
                    }
                    appendExpression(args, leftOperand);
                    for (int i = 1; i < operands.size(); i++) {
                        ExpressionNode expression = operands.get(i);
                        sql.append(',');
                        appendExpression(args, expression);
                    }
                    if (notSingleSubQuery) {
                        sql.append(")");
                    }
                    break;
                }
                case IN: {
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
                            appendExpression(args, expression);
                            join = ',';
                        }
                        if (notSingleSubQuery) {
                            sql.append(")");
                        }
                    }
                    break;
                }
                case BETWEEN: {
                    appendBlank();
                    appendExpression(args, leftOperand);
                    appendOperator(operator);
                    appendBlank();
                    appendExpression(operation.secondOperand());
                    appendOperator(Operator.AND);
                    appendBlank();
                    appendExpression(operation.thirdOperand());
                    break;
                }
                case IS_NULL:
                case IS_NOT_NULL: {
                    appendBlank();
                    if (operator0 != null && operator0.priority()
                                             > operator.priority()) {
                        sql.append('(');
                        appendExpression(args, leftOperand);
                        sql.append(')');
                    } else {
                        appendExpression(args, leftOperand);
                    }
                    appendBlank();
                    appendOperator(operator);
                    break;
                }
                default:
                    throw new UnsupportedOperationException("unknown operator " + operator);
            }
        }

        private void appendOperator(Operator jdbcOperator) {
            String sign = jdbcOperator.sign();
            appendOperator(sign);
        }

        private void appendOperator(String sign) {
            if (Character.isLetter(sign.charAt(0))) {
                appendBlank();
            }
            sql.append(sign);
        }

        protected void appendPaths(Column column) {
            appendBlank();
            StringBuilder sb = sql;
            int iMax = column.size() - 1;
            if (iMax == -1)
                return;
            int i = 0;
            if (column.size() == 1) {
                appendFromAlias().append(".");
            }
            Class<?> type = queryStructure.from().type();

            Column join = ExpressionTrees.column(Lists.of(column.get(0)));

            for (String path : column) {
                EntityType info = mappers.getEntity(type);
                Attribute attribute = info.getAttribute(path);
                if (i++ == iMax) {
                    if (attribute instanceof AnyToOneAttribute) {
                        AnyToOneAttribute joinColumnMapper = (AnyToOneAttribute) attribute;
                        sb.append(LW).append(joinColumnMapper.joinColumnName()).append(RW);
                    } else if (attribute instanceof BasicAttribute) {
                        BasicAttribute basicColumnMapper = (BasicAttribute) attribute;
                        sb.append(LW).append(basicColumnMapper.columnName()).append(RW);
                    } else {
                        throw new IllegalStateException();
                    }
                    return;
                } else {
                    joins.putIfAbsent(join, joins.size());
                    if (i == iMax) {
                        Integer index = joins.get(join);
                        appendTableAttribute(sb, attribute, index).append('.');
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
                Column parent = getParent(k);
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

        private static void appendTable(StringBuilder append, EntityType entityTypeInfo) {
            if (entityTypeInfo instanceof SubSelectType) {
                append.append('(').append(((SubSelectType) entityTypeInfo).subSelectSql()).append(')');
            } else {
                append.append(LW).append(entityTypeInfo.tableName()).append(RW);
            }
        }

        private static Column getParent(Column k) {
            return k.parent();
        }

        Operator getOperator(ExpressionNode expression) {
            return expression instanceof Operation ? ((Operation) expression).operator() : null;
        }

        protected StringBuilder appendTableAttribute(StringBuilder sb, Attribute attribute, Integer index) {
            EntityType information = mappers.getEntity(attribute.javaType());
            String tableName = information.javaType().getSimpleName();
            return appendTableAlias(tableName, index, sb);
        }

        protected Attribute getAttribute(Column path) {
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

        protected void appendOffsetAndLimit(boolean appendOrderBy) {
            int offset = unwrap(queryStructure.offset());
            int limit = unwrap(queryStructure.limit());
            if (offset > 0 || limit >= 0) {
                if (appendOrderBy) {
                    sql.append(" order by (select 0)");
                }
                sql.append(" offset ? rows fetch first ? rows only");
                args.add(Math.max(offset, 0));
                args.add(limit < 0 ? Long.MAX_VALUE : limit);
            }
        }

        private boolean noOrderBy() {
            return queryStructure.orderBy().isEmpty();
        }

        private void appendGroupBy() {
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
                boolean first = true;
                for (Order<?> order : orders) {
                    if (first) {
                        first = false;
                    } else {
                        sql.append(",");
                    }
                    appendExpression(order.expression());
                    sql.append(" ").append(order.order() == SortOrder.DESC ? DESC : ASC);
                }

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
