package io.github.nextentity.test;

import io.github.nextentity.core.QueryConditionBuilder;
import io.github.nextentity.core.QueryStructurePostProcessor;
import io.github.nextentity.core.api.Expression.QueryStructure;
import io.github.nextentity.core.api.LockModeType;
import io.github.nextentity.core.api.Query.QueryStructureBuilder;
import org.junit.jupiter.api.Assertions;

import java.util.List;

public class TestPostProcessor implements QueryStructurePostProcessor {

    @Override
    public QueryStructure preListQuery(QueryConditionBuilder<?, ?> builder, QueryStructure queryStructure) {
        QueryStructureBuilder b = builder.buildMetadata();
        boolean exist = !builder.queryList(b.exist(-1)).isEmpty();
        int count = builder.<Number>queryList(b.count()).get(0).intValue();
        List<?> list = builder.queryList(b.getList(-1, -1, LockModeType.NONE));
        Assertions.assertEquals(list.size(), count);
        Assertions.assertEquals(exist, !list.isEmpty());
        return QueryStructurePostProcessor.super.preListQuery(builder, queryStructure);
    }
}
