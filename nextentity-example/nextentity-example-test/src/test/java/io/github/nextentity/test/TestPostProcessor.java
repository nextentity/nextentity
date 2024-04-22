// package io.github.nextentity.test;
//
// import io.github.nextentity.core.QueryPostProcessor;
// import io.github.nextentity.core.WhereImpl;
// import io.github.nextentity.core.expression.QueryStructure;
// import io.github.nextentity.core.api.LockModeType;
// import org.junit.jupiter.api.Assertions;
//
// import java.util.List;
//
// public class TestPostProcessor implements QueryPostProcessor {
//
//     @Override
//     public QueryStructure preListQuery(WhereImpl<?, ?> builder, QueryStructure queryStructure) {
//         QueryStructureBuilder b = builder.buildMetadata();
//         boolean exist = !builder.queryList(b.exist(-1)).isEmpty();
//         int count = builder.<Number>queryList(b.count()).get(0).intValue();
//         List<?> list = builder.queryList(b.getList(-1, -1, LockModeType.NONE));
//         Assertions.assertEquals(list.size(), count);
//         Assertions.assertEquals(exist, !list.isEmpty());
//         return QueryPostProcessor.super.preListQuery(builder, queryStructure);
//     }
// }
