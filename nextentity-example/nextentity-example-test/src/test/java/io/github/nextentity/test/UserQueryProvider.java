package io.github.nextentity.test;

import io.github.nextentity.core.api.Query;
import io.github.nextentity.core.api.Query.Select;
import io.github.nextentity.core.converter.TypeConverter;
import io.github.nextentity.jdbc.ConnectionProvider;
import io.github.nextentity.jdbc.JdbcQueryExecutor;
import io.github.nextentity.jdbc.JdbcResultCollector;
import io.github.nextentity.jdbc.MySqlQuerySqlBuilder;
import io.github.nextentity.jpa.JpaNativeQueryExecutor;
import io.github.nextentity.jpa.JpaQueryExecutor;
import io.github.nextentity.meta.jpa.JpaMetamodel;
import io.github.nextentity.test.entity.User;
import jakarta.persistence.EntityManager;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Root;
import lombok.Getter;
import lombok.SneakyThrows;
import lombok.experimental.Accessors;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.ArgumentsProvider;

import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Slf4j
public class UserQueryProvider implements ArgumentsProvider {
    public static final Select<User> jdbc = jdbc();
    public static final Select<User> jpa = jpa();
    public static final Select<User> jpaNative = jpaNative();

    @Getter(lazy = true)
    @Accessors(fluent = true)
    private static final List<User> users = loadAllUsers();

    private static List<User> loadAllUsers() {
        EntityManager manager = EntityManagers.getEntityManager();
        CriteriaBuilder builder = manager.getCriteriaBuilder();
        CriteriaQuery<User> query = builder.createQuery(User.class);
        Root<User> root = query.from(User.class);
        query.orderBy(builder.asc(root.get("id")));
        List<User> list = manager.createQuery(query).getResultList();
        Map<Integer, User> map = list.stream().collect(Collectors.toMap(User::getId, Function.identity()));
        return list.stream()
                .map(user -> {
                    user = user.clone();
                    Integer pid = user.getPid();
                    if (pid != null) {
                        User p = map.get(pid);
                        user.setParentUser(p);
                    }
                    return user;
                })
                .collect(Collectors.toList());
    }

    @Override
    public Stream<? extends Arguments> provideArguments(ExtensionContext extensionContext) {
        EntityManagers.getEntityManager().clear();
        return Stream.of(
                Arguments.of(jdbc),
                Arguments.of(jpa),
                Arguments.of(jpaNative)
        );
    }

    private static Select<User> jpa() {
        Query query = jpaQuery();
        log.debug("create jpa query: " + query);
        return query.from(User.class);
    }

    private static Select<User> jpaNative() {
        Query query = jpaNaviveQuery();
        log.debug("create jpa query: " + query);
        return query.from(User.class);
    }

    public static Query jpaQuery() {
        EntityManager manager = EntityManagers.getEntityManager();
        return new JpaQueryExecutor(manager, JpaMetamodel.of(), new MySqlQuerySqlBuilder(), TypeConverter.ofDefault())
                .createQuery(new TestPostProcessor());
    }

    public static Query jpaNaviveQuery() {
        EntityManager manager = EntityManagers.getEntityManager();
        return new JpaNativeQueryExecutor(new MySqlQuerySqlBuilder(), manager, JpaMetamodel.of(), TypeConverter.ofDefault())
                .createQuery(new TestPostProcessor());
    }

    @SneakyThrows
    private static Select<User> jdbc() {
        Query query = jdbcQuery();
        log.debug("create jdbc query: " + query);
        return query.from(User.class);
    }

    public static Query jdbcQuery() {
        ConnectionProvider sqlExecutor = SingleConnectionProvider.CONNECTION_PROVIDER;
        return new JdbcQueryExecutor(JpaMetamodel.of(),
                new MySqlQuerySqlBuilder(),
                sqlExecutor,
                new JdbcResultCollector()
        ).createQuery(new TestPostProcessor());
    }
}
