package io.github.nextentity.test;

import io.github.nextentity.core.EntitiesFactory;
import io.github.nextentity.core.api.Entities;
import io.github.nextentity.test.db.DbConfigs;
import io.github.nextentity.test.entity.UserSummaryMysql;
import io.github.nextentity.test.entity.UserSummarySqlServer;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

/**
 * @author HuangChengwei
 * @since 2024/4/11 下午4:19
 */
public class SubQueryTest {

    @Test
    void testMysql() {
        testMysql(DbConfigs.MYSQL.getJdbcFactory());
        testMysql(DbConfigs.MYSQL.getJpaFactory());
    }

    private static void testMysql(EntitiesFactory entitiesFactory) {
        Entities<String, UserSummaryMysql> summaryEntities = entitiesFactory.getEntities(UserSummaryMysql.class);
        UserSummaryMysql first = summaryEntities.getFirst();
        UserSummaryMysql f2 = summaryEntities.get(first.getUsername());
        Assertions.assertEquals(f2, first);
    }

    // @Test
    // void testSqlserver() {
    //     testSqlserver(DbConfigs.SQLSERVER.getJdbcFactory());
    //     testSqlserver(DbConfigs.SQLSERVER.getJpaFactory());
    // }

    private void testSqlserver(EntitiesFactory entitiesFactory) {
        Entities<String, UserSummarySqlServer> summaryEntities = entitiesFactory.getEntities(UserSummarySqlServer.class);
        UserSummarySqlServer first = summaryEntities.getFirst();
        UserSummarySqlServer f2 = summaryEntities.get(first.getUsername());
        Assertions.assertEquals(f2, first);
    }

}
