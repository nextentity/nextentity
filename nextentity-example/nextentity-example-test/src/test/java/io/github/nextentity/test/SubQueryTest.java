package io.github.nextentity.test;

import io.github.nextentity.api.Repository;
import io.github.nextentity.core.RepositoryFactory;
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

    private static void testMysql(RepositoryFactory entitiesFactory) {
        Repository<String, UserSummaryMysql> summaryEntities = entitiesFactory.getRepository(UserSummaryMysql.class);
        UserSummaryMysql first = summaryEntities.getFirst();
        UserSummaryMysql f2 = summaryEntities.get(first.getUsername());
        Assertions.assertEquals(f2, first);
    }

    // @Test
    // void testSqlserver() {
    //     testSqlserver(DbConfigs.SQLSERVER.getJdbcFactory());
    //     testSqlserver(DbConfigs.SQLSERVER.getJpaFactory());
    // }

    private void testSqlserver(RepositoryFactory entitiesFactory) {
        Repository<String, UserSummarySqlServer> summaryEntities = entitiesFactory.getRepository(UserSummarySqlServer.class);
        UserSummarySqlServer first = summaryEntities.getFirst();
        UserSummarySqlServer f2 = summaryEntities.get(first.getUsername());
        Assertions.assertEquals(f2, first);
    }

}
