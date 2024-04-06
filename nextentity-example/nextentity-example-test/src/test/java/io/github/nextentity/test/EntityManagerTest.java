package io.github.nextentity.test;

import io.github.nextentity.core.api.Query.Select;
import io.github.nextentity.test.entity.UserSummary;
import org.junit.jupiter.api.Test;

import java.util.List;

public class EntityManagerTest {

    @Test
    void test() {
        Select<UserSummary> from = UserQueryProvider.jpaQuery().from(UserSummary.class);
        List<UserSummary> list = from.where(UserSummary::getMaxRandomNumber).le(33).getList();
        System.out.println(list);

    }
}
