package io.github.nextentity.test;

import io.github.nextentity.core.Updaters;
import io.github.nextentity.core.api.Updater;
import io.github.nextentity.jdbc.JdbcUpdate;
import io.github.nextentity.jdbc.MysqlUpdateSqlBuilder;
import io.github.nextentity.jpa.JpaQueryExecutor;
import io.github.nextentity.jpa.JpaUpdate;
import io.github.nextentity.meta.jpa.JpaMetamodel;
import io.github.nextentity.test.entity.User;
import jakarta.persistence.EntityManager;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.ArgumentsProvider;

import java.util.stream.Stream;

public class UserUpdaterProvider implements ArgumentsProvider {
    public static final Updater<User> jdbc = jdbc();
    public static final Updater<User> jpa = jpa();

    @Override
    public Stream<? extends Arguments> provideArguments(ExtensionContext extensionContext) {
        EntityManagers.getEntityManager().clear();
        return Stream.of(
                Arguments.of(jdbc),
                Arguments.of(jpa)
        );
    }

    private static Updater<User> jdbc() {
        JdbcUpdate jdbcUpdate = new JdbcUpdate(
                new MysqlUpdateSqlBuilder(),
                SingleConnectionProvider.CONNECTION_PROVIDER,
                JpaMetamodel.of()
        );
        return Updaters.create(jdbcUpdate, User.class);
    }

    private static Updater<User> jpa() {
        EntityManager em = EntityManagers.getEntityManager();
        JpaQueryExecutor jpaQueryExecutor = new JpaQueryExecutor(em, JpaMetamodel.of(), UserQueryProvider.getJpaQueryExecutor());
        return Updaters.create(new JpaUpdate(em, jpaQueryExecutor), User.class);
    }
}
