package io.github.nextentity.test;

import com.fasterxml.jackson.core.JsonProcessingException;
import io.github.nextentity.core.Tuples;
import io.github.nextentity.core.api.EntityRoot;
import io.github.nextentity.core.api.Expression;
import io.github.nextentity.core.api.Expression.Predicate;
import io.github.nextentity.core.api.ExpressionBuilder.AndOperator;
import io.github.nextentity.core.api.ExpressionTree.QueryStructure;
import io.github.nextentity.core.api.LockModeType;
import io.github.nextentity.core.api.Operator;
import io.github.nextentity.core.api.Path;
import io.github.nextentity.core.api.Query.SliceQueryStructure;
import io.github.nextentity.core.api.Slice;
import io.github.nextentity.core.util.Lists;
import io.github.nextentity.core.util.Paths;
import io.github.nextentity.core.util.tuple.Tuple;
import io.github.nextentity.core.util.tuple.Tuple2;
import io.github.nextentity.core.util.tuple.Tuple3;
import io.github.nextentity.jdbc.JdbcQueryExecutor;
import io.github.nextentity.jdbc.SqlServerQuerySqlBuilder;
import io.github.nextentity.meta.jpa.JpaMetamodel;
import io.github.nextentity.test.db.UserEntities;
import io.github.nextentity.test.entity.User;
import io.github.nextentity.test.projection.UserInterface;
import io.github.nextentity.test.projection.UserModel;
import lombok.extern.slf4j.Slf4j;
import org.jetbrains.annotations.NotNull;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ArgumentsSource;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.OptionalDouble;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static io.github.nextentity.core.util.Paths.get;
import static io.github.nextentity.core.util.Predicates.and;
import static io.github.nextentity.core.util.Predicates.not;
import static io.github.nextentity.core.util.Predicates.or;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrowsExactly;
import static org.junit.jupiter.api.Assertions.assertTrue;

@Slf4j
public class GenericApiTest {

    protected static final String username = "Jeremy Keynes";

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    public void testAndOr(UserEntities userQuery) {
        User single = userQuery
                .where(User::getId).eq(0)
                .orderBy(User::getId).asc()
                .getSingle(10);
        System.out.println(single);
        List<User> dbList = userQuery
                .where(User::getRandomNumber)
                .ne(1)
                .where(User::getRandomNumber)
                .gt(100)
                .where(User::getRandomNumber).ne(125)
                .where(User::getRandomNumber).le(666)
                .where(get(User::getRandomNumber).lt(106)
                        .or(User::getRandomNumber).gt(120)
                        .or(User::getRandomNumber).eq(109)
                )
                .where(User::getRandomNumber).ne(128)
                .getList();

        List<User> ftList = userQuery.users().stream()
                .filter(user -> user.getRandomNumber() != 1
                                && user.getRandomNumber() > 100
                                && user.getRandomNumber() != 125
                                && user.getRandomNumber() <= 666
                                && (user.getRandomNumber() < 106
                                    || user.getRandomNumber() > 120
                                    || user.getRandomNumber() == 109)
                                && user.getRandomNumber() != 128
                )
                .collect(Collectors.toList());

        assertEquals(dbList, ftList);
    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    public void testAndOrChain(UserEntities userQuery) {
        User single = userQuery
                .where(User::getId).eq(0)
                .getSingle();
        System.out.println(single);
        List<User> dbList = userQuery
                .where(User::getRandomNumber).ne(1)
                .where(User::getRandomNumber).gt(100)
                .where(User::getRandomNumber).ne(125)
                .where(User::getRandomNumber).le(666)
                .where(get(User::getRandomNumber).lt(106)
                        .or(User::getRandomNumber).gt(120)
                        .or(User::getRandomNumber).eq(109)
                )
                .where(User::getRandomNumber).ne(128)
                .getList();

        List<User> ftList = userQuery.users().stream()
                .filter(user -> user.getRandomNumber() != 1
                        && user.getRandomNumber() > 100
                        && user.getRandomNumber() != 125
                        && user.getRandomNumber() <= 666
                        && (user.getRandomNumber() < 106
                        || user.getRandomNumber() > 120
                        || user.getRandomNumber() == 109)
                        && user.getRandomNumber() != 128
                )
                .collect(Collectors.toList());

        assertEquals(dbList, ftList);
    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    public void testAndOrChan(UserEntities userQuery) {
        User single = userQuery
                .where(User::getId).eq(0)
                .getSingle();
        System.out.println(single);
        List<User> dbList = userQuery
                .where(User::getRandomNumber).ne(1)
                .where(User::getRandomNumber).gt(100)
                .where(User::getRandomNumber).ne(125)
                .where(User::getRandomNumber).le(666)
                .where(get(User::getRandomNumber).lt(106)
                        .or(User::getRandomNumber).gt(120)
                        .or(User::getRandomNumber).eq(109))
                .where(User::getRandomNumber).ne(128)
                .getList();

        List<User> ftList = userQuery.users().stream()
                .filter(user -> user.getRandomNumber() != 1
                        && user.getRandomNumber() > 100
                        && user.getRandomNumber() != 125
                        && user.getRandomNumber() <= 666
                        && (user.getRandomNumber() < 106
                        || user.getRandomNumber() > 120
                        || user.getRandomNumber() == 109)
                        && user.getRandomNumber() != 128
                )
                .collect(Collectors.toList());

        assertEquals(dbList, ftList);
    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    public void testAndOr2(UserEntities userQuery) {
        User single = userQuery
                .where(get(User::getId).eq(0))
                .getSingle();
        System.out.println(single);
        List<User> dbList = userQuery
                .where(and(
                        get(User::getRandomNumber).ne(1),
                        get(User::getRandomNumber).gt(100),
                        get(User::getRandomNumber).ne(125),
                        get(User::getRandomNumber).le(666),
                        or(
                                get(User::getRandomNumber).lt(106),
                                get(User::getRandomNumber).gt(120),
                                get(User::getRandomNumber).eq(109)
                        ),
                        get(User::getRandomNumber).ne(128)
                )).getList();

        List<User> ftList = userQuery.users().stream()
                .filter(user -> user.getRandomNumber() != 1
                                && user.getRandomNumber() > 100
                                && user.getRandomNumber() != 125
                                && user.getRandomNumber() <= 666
                                && (user.getRandomNumber() < 106
                                    || user.getRandomNumber() > 120
                                    || user.getRandomNumber() == 109)
                                && user.getRandomNumber() != 128
                )
                .collect(Collectors.toList());

        assertEquals(dbList, ftList);
    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    public void testComparablePredicateTesterGt(UserEntities userQuery) {

        List<User> qgt80 = userQuery
                .where(get(User::getRandomNumber).gt(80))
                .orderBy(get(User::getId).asc())
                .getList();
        List<User> fgt80 = userQuery.users().stream()
                .filter(it -> it.getRandomNumber() > 80)
                .collect(Collectors.toList());
        assertEquals(qgt80, fgt80);

    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    public void testPredicateTesterEq(UserEntities userQuery) {
        int userId = 20;
        User user = userQuery
                .fetch(Arrays.asList(
                        get(User::getParentUser),
                        Paths.get(User::getParentUser).get(User::getParentUser)
                ))
                .where(get(User::getId).eq(userId))
                .getSingle();
        assertNotNull(user);
        assertEquals(user.getId(), userId);
        User u = userQuery.users().stream()
                .filter(it -> it.getId() == userId)
                .findAny()
                .orElseThrow();

        if (user.getPid() != null) {
            User parentUser = user.getParentUser();
            assertNotNull(parentUser);
            assertEquals(user.getPid(), parentUser.getId());
            assertEquals(u.getParentUser(), parentUser);
            assertEquals(u.getParentUser().getParentUser(), parentUser.getParentUser());

        }

        List<User> users = userQuery.fetch(
                        User::getParentUser,
                        User::getRandomUser)
                .orderBy(User::getId)
                .getList();

        for (int i = 0; i < users.size(); i++) {
            User u0 = users.get(i);
            User u1 = userQuery.users().get(i);
            assertEquals(u0.getParentUser(), u1.getParentUser());
            assertEquals(u0.getRandomUser(), u1.getRandomUser());
        }


        users = userQuery.fetch(
                        User::getParentUser,
                        User::getRandomUser,
                        User::getTestUser)
                .orderBy(User::getId)
                .getList();

        for (int i = 0; i < users.size(); i++) {
            User u0 = users.get(i);
            User u1 = userQuery.users().get(i);
            assertEquals(u0.getParentUser(), u1.getParentUser());
            assertEquals(u0.getRandomUser(), u1.getRandomUser());
            assertEquals(u0.getTestUser(), u1.getTestUser());
        }

        users = userQuery.fetch(Lists.<Path<User, ?>>of(
                        User::getParentUser,
                        User::getRandomUser,
                        User::getTestUser))
                .orderBy(User::getId)
                .getList();

        for (int i = 0; i < users.size(); i++) {
            User u0 = users.get(i);
            User u1 = userQuery.users().get(i);
            assertEquals(u0.getParentUser(), u1.getParentUser());
            assertEquals(u0.getRandomUser(), u1.getRandomUser());
            assertEquals(u0.getTestUser(), u1.getTestUser());
        }
    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    void testGroupBy(UserEntities userQuery) {
        QueryStructure structure = userQuery
                .select(Arrays.asList(get(User::getId).min(), get(User::getRandomNumber)))
                .where(get(User::isValid).eq(true))
                .groupBy(User::getRandomNumber)
                .having(root -> root.get(User::getRandomNumber).eq(10))
                .buildMetadata()
                .getList(1, 5, LockModeType.PESSIMISTIC_WRITE);
        System.out.println(structure);
        SqlServerQuerySqlBuilder builder = new SqlServerQuerySqlBuilder();
        JdbcQueryExecutor.PreparedSql sql = builder.build(structure, JpaMetamodel.of());
        System.out.println(sql.sql());

        SliceQueryStructure slice = userQuery
                .select(Arrays.asList(get(User::getId).min(), get(User::getRandomNumber)))
                .where(get(User::isValid).eq(true))
                .groupBy(User::getRandomNumber)
                .having(root -> root.get(User::getRandomNumber).eq(10))
                .buildMetadata()
                .slice(1, 5);

        System.out.println(slice);
        System.out.println(slice.count());
        System.out.println(slice.list());
        System.out.println(Operator.AND);


        SliceQueryStructure queryStructure = userQuery
                .select(Arrays.asList(get(User::getId).min(), get(User::getRandomNumber)))
                .where(get(User::isValid).eq(true))
                .groupBy(User::getRandomNumber)
                .having(root -> root.get(User::getRandomNumber).eq(10))
                .orderBy(User::getId)
                .buildMetadata()
                .slice(1, 5);


        SliceQueryStructure queryStructure2 = userQuery
                .select(Arrays.asList(get(User::getId).min(), get(User::getRandomNumber)))
                .where(get(User::isValid).eq(true))
                .groupBy(User::getRandomNumber)
                .having(root -> root.get(User::getRandomNumber).eq(10))
                .orderBy(User::getId).asc()
                .buildMetadata()
                .slice(1, 5);

        assertEquals(queryStructure, queryStructure2);
    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    public void testAggregateFunction(UserEntities userQuery) {

        List<Expression<User, ?>> selected = Arrays.asList(
                get(User::getRandomNumber).min(),
                get(User::getRandomNumber).max(),
                get(User::getRandomNumber).count(),
                get(User::getRandomNumber).avg(),
                get(User::getRandomNumber).sum()
        );
        Tuple aggregated = userQuery
                .select(selected)
                .requireSingle();

        assertNotNull(aggregated);
        assertEquals(getUserIdStream(userQuery).min().orElseThrow(), aggregated.<Integer>get(0));
        assertEquals(getUserIdStream(userQuery).max().orElseThrow(), aggregated.<Integer>get(1));
        assertEquals(getUserIdStream(userQuery).count(), aggregated.<Long>get(2));
        OptionalDouble average = getUserIdStream(userQuery).average();
        assertEquals(average.orElse(0), aggregated.<Number>get(3).doubleValue(), 1);
        assertEquals(getUserIdStream(userQuery).sum(), aggregated.<Number>get(4).intValue());

        List<Tuple> resultList = userQuery
                .select(Arrays.asList(get(User::getId).min(), get(User::getRandomNumber)))
                .where(get(User::isValid).eq(true))
                .groupBy(User::getRandomNumber)
                .getList();

        Map<Integer, Optional<User>> map = userQuery.users().stream()
                .filter(User::isValid)
                .collect(Collectors.groupingBy(User::getRandomNumber, Collectors.minBy(Comparator.comparingInt(User::getId))));

        List<Tuple> fObjects = map.values().stream()
                .map(user -> {
                    Integer userId = user.map(User::getId).orElse(null);
                    Integer randomNumber = user.map(User::getRandomNumber).orElse(null);
                    return Tuples.of(userId, randomNumber);
                })
                .sorted(Comparator.comparing(a -> a.<Integer>get(0)))
                .collect(Collectors.toList());
        assertEquals(new HashSet<>(resultList), new HashSet<>(fObjects));

        Tuple one = userQuery
                .select(Collections.singletonList(get(User::getId).sum()))
                .where(get(User::isValid).eq(true))
                .requireSingle();

        int userId = userQuery.users().stream()
                .filter(User::isValid)
                .mapToInt(User::getId)
                .sum();
        assertEquals(one.<Number>get(0).intValue(), userId);

        Integer first = userQuery
                .select(User::getId)
                .orderBy(get(User::getId).desc())
                .getFirst();
        assertEquals(first, userQuery.users().get(userQuery.users().size() - 1).getId());
    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    public void testSelect(UserEntities userQuery) {
        List<Tuple2<Integer, String>> qList = userQuery
                .select(User::getRandomNumber, User::getUsername)
                .getList();

        List<Tuple2<Integer, String>> fList = userQuery.users().stream()
                .map(it -> Tuples.of(it.getRandomNumber(), it.getUsername()))
                .collect(Collectors.toList());

        assertEquals(qList, fList);

        qList = userQuery
                .selectDistinct(User::getRandomNumber, User::getUsername)
                .getList();
        fList = fList.stream().distinct().collect(Collectors.toList());
        assertEquals(qList.size(), fList.size());
        HashSet<Tuple2<Integer, String>> set = new HashSet<>(qList);
        assertEquals(set.size(), fList.size());
        assertEquals(set, new HashSet<>(fList));
    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    public void testTime(UserEntities userQuery) {
        long start = System.currentTimeMillis();
        userQuery
                .orderBy(Arrays.asList(
                        get(User::getRandomNumber).desc(),
                        get(User::getId).asc()
                ))
                .getList();
        System.out.println(System.currentTimeMillis() - start);
    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    public void testOrderBy(UserEntities userQuery) {
        List<User> list = userQuery
                .orderBy(Arrays.asList(
                        get(User::getRandomNumber).desc(),
                        get(User::getId).asc()
                ))

                .getList();
        ArrayList<User> sorted = new ArrayList<>(userQuery.users());
        sorted.sort((a, b) -> Integer.compare(b.getRandomNumber(), a.getRandomNumber()));
        Iterator<User> ia = list.iterator();
        Iterator<User> ib = sorted.iterator();
        while (ia.hasNext()) {
            User ua = ia.next();
            User ub = ib.next();
            if (!Objects.equals(ua, ub)) {
                boolean equals = ua.equals(ub);
                System.out.println(equals);
            }
        }
        assertEquals(list, sorted);

        list = userQuery
                .orderBy(Arrays.asList(get(User::getUsername).asc(),
                        get(User::getRandomNumber).desc(),
                        get(User::getId).asc()))
                .getList();
        Comparator<User> comparator = Comparator.comparing(User::getUsername)
                .thenComparing(Comparator.comparing(User::getRandomNumber).reversed())
                .thenComparing(User::getId);
        checkOrder(list, comparator);

        list = userQuery
                .orderBy(User::getUsername)
                .orderBy(Arrays.asList(
                        get(User::getRandomNumber).desc(),
                        get(User::getId).asc()
                ))
                .getList();
        checkOrder(list, comparator);

        list = userQuery
                .orderBy((EntityRoot<User> root) -> Arrays.asList(
                        root.get(User::getUsername).asc(),
                        root.get(User::getRandomNumber).desc(),
                        root.get(User::getId).asc()
                ))
                .getList();
        checkOrder(list, comparator);

        list = userQuery
                .orderBy(User::getUsername)
                .orderBy((EntityRoot<User> root) -> Arrays.asList(
                        root.get(User::getRandomNumber).desc(),
                        root.get(User::getId).asc()
                ))
                .getList();
        checkOrder(list, comparator);

        list = userQuery
                .orderBy(get(User::getTime).asc())
                .getList();
        checkOrder(list, Comparator.comparing(User::getTime));
    }

    public <T> void checkOrder(Iterable<T> list, Comparator<T> comparator) {
        Iterator<T> iterator = list.iterator();
        if (!iterator.hasNext()) {
            return;
        }
        T pre = iterator.next();
        while (iterator.hasNext()) {
            T next = iterator.next();
            int compare = comparator.compare(pre, next);
            if (compare > 0) {
                System.out.println();
            }
            assertTrue(compare <= 0);
            pre = next;
        }
    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    public void testOrderBy2(UserEntities userQuery) {
        List<User> list = userQuery
                .orderBy(
                        get(User::getRandomNumber).desc(),
                        get(User::getId).asc()
                )
                .getList();
        Comparator<User> comparator = Comparator
                .comparing(User::getRandomNumber)
                .reversed()
                .thenComparing(User::getId);
        checkOrder(list, comparator);

        list = userQuery
                .orderBy(
                        get(User::getUsername).asc(),
                        get(User::getRandomNumber).desc(),
                        get(User::getId).asc()
                )
                .getList();

        comparator = Comparator
                .comparing(User::getUsername)
                .thenComparing(Comparator.comparing(User::getRandomNumber).reversed())
                .thenComparing(User::getId);
        checkOrder(list, comparator);

        list = userQuery
                .orderBy(User::getUsername)
                .orderBy(User::getRandomNumber).desc()
                .orderBy(User::getId).asc()
                .getList();
        checkOrder(list, comparator);

        list = userQuery
                .orderBy(User::getUsername, User::getRandomNumber, User::getId).asc()
                .getList();

        comparator = Comparator
                .comparing(User::getUsername)
                .thenComparing(User::getRandomNumber)
                .thenComparing(User::getId);
        checkOrder(list, comparator);

        list = userQuery
                .orderBy(User::getUsername, User::getRandomNumber, User::getId).desc()
                .getList();

        comparator = Comparator
                .comparing(User::getUsername)
                .thenComparing(User::getRandomNumber)
                .thenComparing(User::getId)
                .reversed();
        checkOrder(list, comparator);

        list = userQuery
                .orderBy(User::getTime)
                .getList();
        comparator = Comparator
                .comparing(User::getTime);
        checkOrder(list, comparator);
    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    public void testPredicate(UserEntities userQuery) {
        List<User> qList = userQuery
                .where(not(get(User::getRandomNumber).ge(10)
                        .or(User::getRandomNumber).lt(5)))
                .getList();
        List<User> fList = userQuery.users().stream()
                .filter(it -> !(it.getRandomNumber() >= 10 || it.getRandomNumber() < 5))
                .collect(Collectors.toList());

        assertEquals(qList, fList);

        qList = userQuery
                .where(get(User::getUsername).ne("Jeremy Keynes").not())
                .getList();
        fList = userQuery.users().stream()
                .filter(it -> (it.getUsername().equalsIgnoreCase("Jeremy Keynes")))
                .collect(Collectors.toList());
        assertEquals(qList, fList);

        qList = userQuery.where(get(User::getUsername).eq("Jeremy Keynes"))
                .getList();
        assertEquals(qList, fList);

        qList = userQuery.where(
                        not(get(User::getUsername).eq("Jeremy Keynes")
                                .or(get(User::getId).eq(3)))
                )
                .getList();
        fList = userQuery.users().stream()
                .filter(it -> !(it.getUsername().equalsIgnoreCase("Jeremy Keynes")
                        || it.getId() == 3))
                .collect(Collectors.toList());
        assertEquals(qList, fList);

        qList = userQuery
                .where(not(get(User::getUsername).eq("Jeremy Keynes")
                        .and(get(User::getId).eq(3))
                ))
                .getList();
        fList = userQuery.users().stream()
                .filter(it -> !(it.getUsername().equalsIgnoreCase("Jeremy Keynes")
                                && it.getId() == 3))
                .collect(Collectors.toList());
        assertEquals(qList, fList);

    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    public void testPredicate2(UserEntities userQuery) {
        List<User> qList = userQuery
                .where(or(
                        get(User::getRandomNumber).ge(10),
                        get(User::getRandomNumber).lt(5)
                ).not())
                .getList();
        List<User> fList = userQuery.users().stream()
                .filter(it -> !(it.getRandomNumber() >= 10 || it.getRandomNumber() < 5))
                .collect(Collectors.toList());

        assertEquals(qList, fList);

        qList = userQuery
                .where(get(User::getUsername).eq("Jeremy Keynes").not())
                .getList();
        fList = userQuery.users().stream()
                .filter(it -> !(it.getUsername().equalsIgnoreCase("Jeremy Keynes")))
                .collect(Collectors.toList());
        assertEquals(qList, fList);

        qList = userQuery.where(get(User::getUsername).eq("Jeremy Keynes")
                        .not()
                )
                .getList();
        assertEquals(qList, fList);

        qList = userQuery.where(not(get(User::getUsername).eq("Jeremy Keynes")
                        .or(get(User::getId).eq(3))
                ))
                .getList();
        fList = userQuery.users().stream()
                .filter(it -> !(it.getUsername().equalsIgnoreCase("Jeremy Keynes")
                                || it.getId() == 3))
                .collect(Collectors.toList());
        assertEquals(qList, fList);

        qList = userQuery
                .where(and(
                        get(User::getUsername).eq("Jeremy Keynes"),
                        get(User::getId).eq(3)
                ).not())
                .getList();
        fList = userQuery.users().stream()
                .filter(it -> !(it.getUsername().equalsIgnoreCase("Jeremy Keynes")
                                && it.getId() == 3))
                .collect(Collectors.toList());
        assertEquals(qList, fList);

    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    public void testGroupBy1(UserEntities userQuery) {
        List<Tuple3<Boolean, Integer, Integer>> resultList = userQuery
                .select(User::isValid, User::getRandomNumber, User::getPid)
                .groupBy(User::getRandomNumber, User::getPid, User::isValid)
                .getList();

        List<Tuple3<Boolean, Integer, Integer>> resultList2 = userQuery
                .select(User::isValid, User::getRandomNumber, User::getPid)
                .groupBy(User::getRandomNumber, User::getPid, User::isValid)
                .getList();
        assertEquals(resultList, resultList2);
        List<Tuple3<Boolean, Integer, Integer>> list = userQuery.users().stream()
                .map(it -> Tuples.of(it.isValid(), it.getRandomNumber(), it.getPid()))
                .distinct()
                .collect(Collectors.toList());
        assertEquals(sort(resultList), sort(list));


    }

    @NotNull
    private static List<Tuple3<Boolean, Integer, Integer>> sort(List<Tuple3<Boolean, Integer, Integer>> resultList) {
        return resultList.stream()
                .sorted(Comparator.comparing(Object::toString))
                .collect(Collectors.toList());
    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    public void testIsNull(UserEntities userQuery) {

        List<User> qList = userQuery.where(get(User::getPid).isNotNull())
                .getList();

        List<User> fList = userQuery.users().stream()
                .filter(it -> it.getPid() != null)
                .collect(Collectors.toList());
        assertEquals(qList, fList);

        qList = userQuery.where(get(User::getPid).add(2).multiply(3).isNull())
                .getList();

        fList = userQuery.users().stream()
                .filter(it -> it.getPid() == null)
                .collect(Collectors.toList());
        assertEquals(qList, fList);

    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    public void testOperatorIfNotNull(UserEntities userQuery) {
        List<User> qList = userQuery.where(User::getRandomNumber).eq(10).getList();
        List<User> fList = userQuery.users().stream().filter(u -> u.getRandomNumber() == 10)
                .collect(Collectors.toList());
        assertEquals(qList, fList);


        qList = userQuery
                .where(User::getRandomUser).eqIfNotNull(null)
                .where(User::getRandomUser).eqIfNotNull(null)
                .where(User::getRandomNumber).gtIfNotNull(null)
                .where(User::getRandomNumber).geIfNotNull(null)
                .where(User::getId).eqIfNotNull(null)
                .where(User::getUsername).eqIfNotNull(null).getList();
        assertEquals(qList, userQuery.users());


        qList = userQuery.where(get(User::getRandomNumber).eqIfNotNull(null)).getList();
        assertEquals(qList, userQuery.users());
        qList = userQuery.where(get(User::getRandomNumber).gtIfNotNull(null)).getList();
        assertEquals(qList, userQuery.users());
        qList = userQuery.where(get(User::getRandomNumber).geIfNotNull(null)).getList();
        assertEquals(qList, userQuery.users());
        qList = userQuery.where(get(User::getRandomNumber).ltIfNotNull(null)).getList();
        assertEquals(qList, userQuery.users());
        qList = userQuery.where(get(User::getRandomNumber).leIfNotNull(null)).getList();
        assertEquals(qList, userQuery.users());


        qList = userQuery.where(get(User::getRandomNumber).eqIfNotNull(20)).getList();
        fList = userQuery.users().stream().filter(u -> u.getRandomNumber() == 20)
                .collect(Collectors.toList());
        assertEquals(qList, fList);

        AndOperator<User> predicate = get(User::getRandomNumber).eq(20).and(User::getUsername).eqIfNotNull(null);
        qList = userQuery.where(predicate).getList();

        assertEquals(qList, fList);
        qList = userQuery.where(get(User::getRandomNumber).gtIfNotNull(20)).getList();
        fList = userQuery.users().stream().filter(u -> u.getRandomNumber() > 20)
                .collect(Collectors.toList());
        assertEquals(qList, fList);
        qList = userQuery.where(get(User::getRandomNumber).geIfNotNull(20)).getList();
        fList = userQuery.users().stream().filter(u -> u.getRandomNumber() >= 20)
                .collect(Collectors.toList());
        assertEquals(qList, fList);
        qList = userQuery.where(get(User::getRandomNumber).ltIfNotNull(20)).getList();
        fList = userQuery.users().stream().filter(u -> u.getRandomNumber() < 20)
                .collect(Collectors.toList());
        assertEquals(qList, fList);
        qList = userQuery.where(get(User::getRandomNumber).leIfNotNull(20)).getList();
        fList = userQuery.users().stream().filter(u -> u.getRandomNumber() <= 20)
                .collect(Collectors.toList());
        assertEquals(qList, fList);

        qList = userQuery
                .where(User::getRandomUser).eqIfNotNull(null)
                .where(User::getId).eqIfNotNull(null)
                .where(User::getUsername).eqIfNotNull(null)
                .where(User::getRandomNumber).eq(10)
                .getList();
        fList = userQuery.users().stream().filter(u -> u.getRandomNumber() == 10)
                .collect(Collectors.toList());
        assertEquals(qList, fList);

        qList = userQuery
                .where(User::getRandomUser).eqIfNotNull(null)
                .where(User::getId).eqIfNotNull(null)
                .where(User::getUsername).eqIfNotNull(null)
                .where(User::getRandomNumber).eqIfNotNull(10)
                .getList();
        assertEquals(qList, fList);

        qList = userQuery
                .where(User::getRandomUser).eqIfNotNull(null)
                .where(User::getId).eqIfNotNull(null)
                .where(User::getUsername).eqIfNotNull(null)
                .where(User::getRandomNumber).geIfNotNull(10)
                .getList();
        fList = userQuery.users().stream().filter(u -> u.getRandomNumber() >= 10)
                .collect(Collectors.toList());
        assertEquals(qList, fList);

        qList = userQuery
                .where(User::getRandomUser).eqIfNotNull(null)
                .where(User::getId).eqIfNotNull(null)
                .where(User::getUsername).eqIfNotNull(null)
                .where(User::getRandomNumber).gtIfNotNull(10)
                .getList();
        fList = userQuery.users().stream().filter(u -> u.getRandomNumber() > 10)
                .collect(Collectors.toList());
        assertEquals(qList, fList);


        qList = userQuery
                .where(User::getRandomUser).eqIfNotNull(null)
                .where(User::getId).eqIfNotNull(null)
                .where(User::getUsername).eqIfNotNull(null)
                .where(User::getRandomNumber).leIfNotNull(10)
                .getList();
        fList = userQuery.users().stream().filter(u -> u.getRandomNumber() <= 10)
                .collect(Collectors.toList());
        assertEquals(qList, fList);


        qList = userQuery
                .where(User::getRandomUser).eqIfNotNull(null)
                .where(User::getId).addIfNotNull(null).eqIfNotNull(null)
                .where(User::getId).subtractIfNotNull(null).eqIfNotNull(null)
                .where(User::getId).multiplyIfNotNull(null).eqIfNotNull(null)
                .where(User::getId).divideIfNotNull(null).eqIfNotNull(null)
                .where(User::getId).modIfNotNull(null).eqIfNotNull(null)
                .where(User::getUsername).eqIfNotNull(null)
                .where(User::getRandomNumber).ltIfNotNull(10)
                .getList();
        fList = userQuery.users().stream().filter(u -> u.getRandomNumber() < 10)
                .collect(Collectors.toList());
        assertEquals(qList, fList);

        qList = userQuery
                .where(User::getRandomUser).eqIfNotNull(null)
                .where(User::getId).addIfNotNull(null).eqIfNotNull(null)
                .where(User::getId).subtractIfNotNull(null).eqIfNotNull(null)
                .where(User::getId).multiplyIfNotNull(null).eqIfNotNull(null)
                .where(User::getId).divideIfNotNull(null).eqIfNotNull(null)
                .where(User::getId).modIfNotNull(null).eqIfNotNull(null)
                .where(User::getUsername).eqIfNotNull(null)
                .where(User::getRandomNumber).addIfNotNull(null).ltIfNotNull(10)
                .getList();
        fList = userQuery.users().stream().filter(u -> u.getRandomNumber() < 10)
                .collect(Collectors.toList());
        assertEquals(qList, fList);
        qList = userQuery
                .where(User::getRandomUser).eqIfNotNull(null)
                .where(User::getId).addIfNotNull(null).eqIfNotNull(null)
                .where(User::getId).subtractIfNotNull(null).eqIfNotNull(null)
                .where(User::getId).multiplyIfNotNull(null).eqIfNotNull(null)
                .where(User::getId).divideIfNotNull(null).eqIfNotNull(null)
                .where(User::getId).modIfNotNull(null).eqIfNotNull(null)
                .where(User::getUsername).eqIfNotNull(null)
                .where(User::getRandomNumber).addIfNotNull(3).ltIfNotNull(10)
                .getList();
        fList = userQuery.users().stream().filter(u -> u.getRandomNumber() + 3 < 10)
                .collect(Collectors.toList());
        assertEquals(qList, fList);
        qList = userQuery
                .where(User::getRandomUser).eqIfNotNull(null)
                .where(User::getId).addIfNotNull(null).eqIfNotNull(null)
                .where(User::getId).subtractIfNotNull(null).eqIfNotNull(null)
                .where(User::getId).multiplyIfNotNull(null).eqIfNotNull(null)
                .where(User::getId).divideIfNotNull(null).eqIfNotNull(null)
                .where(User::getId).modIfNotNull(null).eqIfNotNull(null)
                .where(User::getUsername).eqIfNotNull(null)
                .where(User::getRandomNumber).addIfNotNull(3).ltIfNotNull(10)
                .getList();
        assertEquals(qList, fList);

        qList = userQuery
                .where(User::getRandomUser).eqIfNotNull(null)
                .where(User::getId).addIfNotNull(null).eqIfNotNull(null)
                .where(User::getId).subtractIfNotNull(null).eqIfNotNull(null)
                .where(User::getId).multiplyIfNotNull(null).eqIfNotNull(null)
                .where(User::getId).divideIfNotNull(null).eqIfNotNull(null)
                .where(User::getId).modIfNotNull(null).eqIfNotNull(null)
                .where(User::getUsername).eqIfNotNull(null)
                .where(User::getRandomNumber).multiplyIfNotNull(3).ltIfNotNull(50)
                .getList();
        fList = userQuery.users().stream().filter(u -> u.getRandomNumber() * 3 < 50)
                .collect(Collectors.toList());
        assertEquals(qList, fList);

        qList = userQuery
                .where(User::getRandomUser).eqIfNotNull(null)
                .where(User::getId).addIfNotNull(null).eqIfNotNull(null)
                .where(User::getId).subtractIfNotNull(null).eqIfNotNull(null)
                .where(User::getId).multiplyIfNotNull(null).eqIfNotNull(null)
                .where(User::getId).divideIfNotNull(null).eqIfNotNull(null)
                .where(User::getId).modIfNotNull(null).eqIfNotNull(null)
                .where(User::getUsername).eqIfNotNull(null)
                .where(User::getRandomNumber).divideIfNotNull(3).ltIfNotNull(10)
                .getList();
        fList = userQuery.users().stream().filter(u -> u.getRandomNumber() / 3.0 < 10)
                .collect(Collectors.toList());
        assertEquals(qList, fList);

    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    public void testOperator2(UserEntities userQuery) {
        Predicate<User> isValid = get(User::isValid);
        userQuery.where(isValid
                        .and(User::getRandomNumber).notBetween(10, 15)
                        .and(User::getId).mod(3).eq(0)
                )
                .getList();
    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    public void testOperator(UserEntities userQuery) {

        Predicate<User> isValid = get(User::isValid);
        List<User> qList = userQuery.where(isValid).getList();
        List<User> validUsers = userQuery.users().stream().filter(User::isValid)
                .collect(Collectors.toList());
        List<User> fList = validUsers;
        assertEquals(qList, fList);

        qList = userQuery.where(isValid.and(User::getRandomNumber).eq(2))
                .getList();
        fList = validUsers.stream().filter(user -> user.getRandomNumber() == 2)
                .collect(Collectors.toList());
        assertEquals(qList, fList);

        qList = userQuery.where(isValid.and(User::getPid).ne(2))
                .getList();
        fList = validUsers.stream().filter(user -> user.getPid() != null && user.getPid() != 2)
                .collect(Collectors.toList());
        assertEquals(qList, fList);

        qList = userQuery.where(isValid.and(User::getRandomNumber).in(1, 2, 3))
                .getList();
        fList = validUsers.stream().filter(user -> Arrays.asList(1, 2, 3).contains(user.getRandomNumber()))
                .collect(Collectors.toList());
        assertEquals(qList, fList);

        qList = userQuery.where(isValid.and(User::getRandomNumber).notIn(1, 2, 3))
                .getList();
        fList = validUsers.stream().filter(user -> !Arrays.asList(1, 2, 3).contains(user.getRandomNumber()))
                .collect(Collectors.toList());
        assertEquals(qList, fList);

        qList = userQuery.where(isValid.and(User::getPid).isNull())
                .getList();
        fList = validUsers.stream().filter(user -> user.getPid() == null)
                .collect(Collectors.toList());
        assertEquals(qList, fList);

        qList = userQuery.where(isValid.and(User::getRandomNumber).ge(10))
                .getList();
        fList = validUsers.stream().filter(user -> user.getRandomNumber() >= 10)
                .collect(Collectors.toList());
        assertEquals(qList, fList);

        qList = userQuery.where(isValid.and(User::getRandomNumber).gt(10))
                .getList();
        fList = validUsers.stream().filter(user -> user.getRandomNumber() > 10)
                .collect(Collectors.toList());
        assertEquals(qList, fList);

        qList = userQuery.where(isValid.and(User::getRandomNumber).le(10))
                .getList();
        fList = validUsers.stream().filter(user -> user.getRandomNumber() <= 10)
                .collect(Collectors.toList());
        assertEquals(qList, fList);

        qList = userQuery.where(isValid.and(User::getRandomNumber).lt(10))
                .getList();
        fList = validUsers.stream().filter(user -> user.getRandomNumber() < 10)
                .collect(Collectors.toList());
        assertEquals(qList, fList);

        qList = userQuery.where(isValid.and(User::getRandomNumber).between(10, 15))
                .getList();
        fList = validUsers.stream().filter(user -> user.getRandomNumber() >= 10 && user.getRandomNumber() <= 15)
                .collect(Collectors.toList());
        assertEquals(qList, fList);

        qList = userQuery.where(isValid.and(User::getRandomNumber).notBetween(10, 15))
                .getList();
        fList = validUsers.stream().filter(user -> user.getRandomNumber() < 10 || user.getRandomNumber() > 15)
                .collect(Collectors.toList());
        assertEquals(qList, fList);

        qList = userQuery.where(isValid
                        .and(User::getRandomNumber).notBetween(10, 15)
                        .and(User::getId).mod(3).eq(0)
                )
                .getList();
        fList = validUsers.stream().filter(user ->
                        !(user.getRandomNumber() >= 10 && user.getRandomNumber() <= 15)
                                && user.getId() % 3 == 0)
                .collect(Collectors.toList());
        assertEquals(qList, fList);

        qList = userQuery.where(isValid.and(User::getRandomNumber).ge(get(User::getPid)))
                .getList();
        fList = validUsers.stream().filter(user -> user.getPid() != null && user.getRandomNumber() >= user.getPid())
                .collect(Collectors.toList());
        assertEquals(qList, fList);

        qList = userQuery.where(isValid.and(User::getRandomNumber).gt(get(User::getPid)))
                .getList();
        fList = validUsers.stream().filter(user -> user.getPid() != null && user.getRandomNumber() > user.getPid())
                .collect(Collectors.toList());
        assertEquals(qList, fList);

        qList = userQuery.where(isValid.and(User::getRandomNumber).le(get(User::getPid)))
                .getList();
        fList = validUsers.stream().filter(user -> user.getPid() != null && user.getRandomNumber() <= user.getPid())
                .collect(Collectors.toList());
        assertEquals(qList, fList);

        qList = userQuery.where(isValid.and(User::getRandomNumber).lt(get(User::getPid)))
                .getList();
        fList = validUsers.stream().filter(user -> user.getPid() != null && user.getRandomNumber() < user.getPid())
                .collect(Collectors.toList());
        assertEquals(qList, fList);

        qList = userQuery.where(isValid.and(User::getRandomNumber)
                        .between(get(User::getRandomNumber), get(User::getPid)))
                .getList();
        fList = validUsers.stream()
                .filter(user -> user.getPid() != null && user.getRandomNumber() >= user.getRandomNumber() && user.getRandomNumber() <= user.getPid())
                .collect(Collectors.toList());
        assertEquals(qList, fList);

    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    public void testPredicateAssembler(UserEntities userQuery) {

        List<User> qList = userQuery.where(get(User::isValid).eq(true)
                        .and(User::getParentUser).get(User::getUsername).eq(username))
                .getList();
        List<User> fList = userQuery.users().stream()
                .filter(user -> user.isValid()
                                && user.getParentUser() != null
                                && Objects.equals(user.getParentUser().getUsername(), username))
                .collect(Collectors.toList());

        assertEquals(qList, fList);
        qList = userQuery.where(User::isValid).eq(true)
                .where(User::getParentUser).get(User::getParentUser).get(User::getUsername).eq(username)
                .getList();
        fList = userQuery.users().stream()
                .filter(user -> user.isValid()
                                && user.getParentUser() != null
                                && user.getParentUser().getParentUser() != null
                                && Objects.equals(user.getParentUser().getParentUser().getUsername(), username))
                .collect(Collectors.toList());
        assertEquals(qList, fList);

        Path<User, Number> getUsername = User::getRandomNumber;
        qList = userQuery.where(get(User::isValid).eq(true)
                        .and(getUsername).eq(10))
                .getList();
        fList = userQuery.users().stream()
                .filter(user -> user.isValid()
                                && Objects.equals(user.getRandomNumber(), 10))
                .collect(Collectors.toList());

        assertEquals(qList, fList);

        qList = userQuery.where(get(User::isValid).eq(true)
                        .or(getUsername).eq(10))
                .getList();
        fList = userQuery.users().stream()
                .filter(user -> user.isValid()
                                || Objects.equals(user.getRandomNumber(), 10))
                .collect(Collectors.toList());

        assertEquals(qList, fList);

        qList = userQuery.where(get(User::isValid).eq(true)
                        .and(getUsername).ne(10))
                .getList();
        fList = userQuery.users().stream()
                .filter(user -> user.isValid()
                                && !Objects.equals(user.getRandomNumber(), 10))
                .collect(Collectors.toList());

        assertEquals(qList, fList);

        qList = userQuery.where(get(User::isValid).eq(true)
                        .or(getUsername).ne(10))
                .getList();
        fList = userQuery.users().stream()
                .filter(user -> user.isValid()
                                || !Objects.equals(user.getRandomNumber(), 10))
                .collect(Collectors.toList());

        assertEquals(qList, fList);

        Date time = userQuery.users().get(20).getTime();

        Expression<User, Boolean> or = get(User::isValid).eq(true)
                .or(
                        Paths.get(User::getParentUser)
                                .get(User::getUsername)
                                .eq(username)
                                .and(User::getTime)
                                .ge(time));
        qList = userQuery.where(or).getList();

        List<User> jeremy_keynes = userQuery
                .fetch(User::getParentUser)
                .where(get(User::isValid).eq(true)
                        .or(Paths.get(User::getParentUser)
                                .get(User::getUsername).eq(username)
                                .and(User::getTime).ge(time)
                        ))
                .getList();

        fList = userQuery.users().stream()
                .filter(user -> user.isValid()
                                || (user.getParentUser() != null
                                    && Objects.equals(user.getParentUser().getUsername(), username)
                                    && user.getTime().getTime() >= time.getTime()))
                .collect(Collectors.toList());

        assertEquals(qList, fList);
        assertEquals(qList, jeremy_keynes);

        qList = userQuery.where(get(User::isValid).eq(true)
                        .and(User::getRandomNumber).ne(5))
                .getList();
        fList = userQuery.users().stream()
                .filter(user -> user.isValid()
                                && user.getRandomNumber() != 5)
                .collect(Collectors.toList());

        assertEquals(qList, fList);

        qList = userQuery.where(get(User::isValid).eq(true)
                        .or(User::getRandomNumber).eq(5))
                .getList();
        fList = userQuery.users().stream()
                .filter(user -> user.isValid()
                                || user.getRandomNumber() == 5)
                .collect(Collectors.toList());

        assertEquals(qList, fList);

        qList = userQuery.where(get(User::getRandomNumber).ne(6)
                        .or(User::isValid).eq(false))
                .getList();
        fList = userQuery.users().stream()
                .filter(user -> user.getRandomNumber() != 6
                                || !user.isValid())
                .collect(Collectors.toList());

        assertEquals((qList), (fList));

        qList = userQuery.where(get(User::getRandomNumber).ne(6)
                        .and(User::getParentUser).get(User::isValid).eq(true))
                .getList();
        fList = userQuery.users().stream()
                .filter(user -> user.getRandomNumber() != 6
                                && (user.getParentUser() != null && user.getParentUser().isValid()))
                .collect(Collectors.toList());

        assertEquals((qList), (fList));

        qList = userQuery.where(get(User::getRandomNumber).ne(6)
                        .and(User::getParentUser).get(User::isValid).ne(true))
                .getList();
        fList = userQuery.users().stream()
                .filter(user -> user.getRandomNumber() != 6
                                && (user.getParentUser() != null && !user.getParentUser().isValid()))
                .collect(Collectors.toList());

        assertEquals((qList), (fList));

        qList = userQuery.where(get(User::getRandomNumber).ne(6)
                        .or(User::getParentUser).get(User::isValid).ne(true))
                .getList();
        fList = userQuery.users().stream()
                .filter(user -> user.getRandomNumber() != 6
                                || (user.getParentUser() != null && !user.getParentUser().isValid()))
                .collect(Collectors.toList());

        assertEquals((qList), (fList));

        qList = userQuery.where(not(get(User::getRandomNumber).ge(10)
                        .or(User::getRandomNumber).lt(5)
                ))
                .getList();
        fList = userQuery.users().stream()
                .filter(it -> !(it.getRandomNumber() >= 10 || it.getRandomNumber() < 5))
                .collect(Collectors.toList());

        assertEquals(qList, fList);

        qList = userQuery.where(not(get(User::getRandomNumber).ge(10)
                                .and(User::getRandomNumber).le(15)
                        )
                )
                .getList();
        fList = userQuery.users().stream()
                .filter(it -> !(it.getRandomNumber() >= 10 && it.getRandomNumber() <= 15))
                .collect(Collectors.toList());

        assertEquals(qList, fList);

        qList = userQuery.where(not(
                        get(User::getRandomNumber).ge(10)
                                .and(User::getUsername).eq(username)
                ))
                .getList();
        fList = userQuery.users().stream()
                .filter(it -> !(it.getRandomNumber() >= 10 && it.getUsername().equals(username)))
                .collect(Collectors.toList());
        assertEquals(qList, fList);

        qList = userQuery.where(not(get(User::getRandomNumber).ge(10)
                                .or(User::getUsername).eq(username)
                        )
                )
                .getList();
        fList = userQuery.users().stream()
                .filter(it -> !(it.getRandomNumber() >= 10 || it.getUsername().equals(username)))
                .collect(Collectors.toList());
        assertEquals(qList, fList);

        qList = userQuery.where(not(get(User::getRandomNumber).ge(10)
                        .and(User::getUsername).eq(username))
                        .not()
                )
                .getList();
        fList = userQuery.users().stream()
                .filter(it -> (it.getRandomNumber() >= 10 && it.getUsername().equals(username)))
                .collect(Collectors.toList());
        assertEquals(qList, fList);

        qList = userQuery.where(not(get(User::getRandomNumber).ge(10)
                        .or(User::getUsername).eq(username))
                        .not()
                )
                .getList();
        fList = userQuery.users().stream()
                .filter(it -> it.getRandomNumber() >= 10 || it.getUsername().equals(username))
                .collect(Collectors.toList());
        assertEquals(qList, fList);

    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    void testSubQuery(UserEntities userQuery) {
        Date time = userQuery.users().get(20).getTime();

        userQuery
                .fetch(User::getParentUser)
                .where(get(User::isValid).eq(true)
                        .or(Paths.get(User::getParentUser)
                                .get(User::getUsername).eq(username)
                                .and(User::getTime).ge(time)
                        ))
                .count();
    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    public void testNumberPredicateTester(UserEntities userQuery) {
        List<User> list = userQuery
                .where(get(User::getRandomNumber).add(2).ge(4))
                .getList();
        List<User> fList = userQuery.users().stream()
                .filter(user -> user.getRandomNumber() + 2 >= 4)
                .collect(Collectors.toList());

        assertEquals(list, fList);

        list = userQuery
                .where(get(User::getRandomNumber).subtract(2).ge(4))
                .getList();
        fList = userQuery.users().stream()
                .filter(user -> user.getRandomNumber() - 2 >= 4)
                .collect(Collectors.toList());

        assertEquals(list, fList);

        list = userQuery
                .where(get(User::getRandomNumber).multiply(2).ge(4))
                .getList();
        fList = userQuery.users().stream()
                .filter(user -> user.getRandomNumber() * 2 >= 4)
                .collect(Collectors.toList());

        assertEquals(list, fList);

        list = userQuery
                .where(get(User::getRandomNumber).divide(2).ge(4))
                .getList();
        fList = userQuery.users().stream()
                .filter(user -> user.getRandomNumber() / 2 >= 4)
                .collect(Collectors.toList());

        assertEquals(list, fList);

        list = userQuery
                .where(get(User::getRandomNumber).mod(2).ge(1))
                .getList();
        fList = userQuery.users().stream()
                .filter(user -> user.getRandomNumber() % 2 == 1)
                .collect(Collectors.toList());

        assertEquals(list, fList);

        ///
        list = userQuery
                .where(get(User::getRandomNumber).add(get(User::getId)).ge(40))
                .getList();
        fList = userQuery.users().stream()
                .filter(user -> user.getRandomNumber() + user.getId() >= 40)
                .collect(Collectors.toList());

        assertEquals(list, fList);

        list = userQuery
                .where(get(User::getRandomNumber).subtract(get(User::getId)).ge(40))
                .getList();
        fList = userQuery.users().stream()
                .filter(user -> user.getRandomNumber() - user.getId() >= 40)
                .collect(Collectors.toList());

        assertEquals(list, fList);

        list = userQuery
                .where(get(User::getRandomNumber).multiply(get(User::getId)).ge(40))
                .getList();
        fList = userQuery.users().stream()
                .filter(user -> user.getRandomNumber() * user.getId() >= 40)
                .collect(Collectors.toList());

        assertEquals(list, fList);

        list = userQuery
                .where(get(User::getRandomNumber).divide(get(User::getId)).ge(40))
                .where(User::getId).ne(0)
                .getList();
        fList = userQuery.users().stream()
                .filter(user -> user.getId() != 0 && user.getRandomNumber() / user.getId() >= 40)
                .collect(Collectors.toList());

        assertEquals(list, fList);

        list = userQuery
                .where(get(User::getRandomNumber).mod(get(User::getId)).ge(10))
                .where(User::getId).ne(0)
                .getList();
        fList = userQuery.users().stream()
                .filter(user -> user.getId() != 0 && user.getRandomNumber() % user.getId() >= 10)
                .collect(Collectors.toList());

        assertEquals(list, fList);

    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    public void testStringPredicateTester(UserEntities userQuery) {
        String username = "Roy Sawyer";

        List<User> qList = userQuery.where(get(User::getUsername).substring(2).eq("eremy Keynes"))
                .getList();
        List<User> fList = userQuery.users().stream()
                .filter(user -> user.getUsername().substring(1).equals("eremy Keynes"))
                .collect(Collectors.toList());

        assertEquals(qList, fList);

        qList = userQuery.where(get(User::getUsername).substring(1, 1).eq("M"))
                .getList();
        fList = userQuery.users().stream()
                .filter(user -> user.getUsername().charAt(0) == 'M')
                .collect(Collectors.toList());

        assertEquals(qList, fList);

        qList = userQuery.where(get(User::getUsername).trim().like(username))
                .getList();
        fList = userQuery.users().stream()
                .filter(user -> user.getUsername().trim().startsWith(username))
                .collect(Collectors.toList());
        assertEquals(qList, fList);

        qList = userQuery.where(get(User::getUsername).trim().notContains("i"))
                .getList();
        fList = userQuery.users().stream()
                .filter(user -> !user.getUsername().toLowerCase().contains("i"))
                .collect(Collectors.toList());
        assertEquals(qList, fList);

        qList = userQuery.where(get(User::getUsername).length().eq(username.length()))
                .getList();
        fList = userQuery.users().stream()
                .filter(user -> user.getUsername().length() == username.length())
                .collect(Collectors.toList());
        assertEquals(qList, fList);

        qList = userQuery.where(get(User::getUsername).startWith("M"))
                .getList();
        fList = userQuery.users().stream()
                .filter(user -> user.getUsername().startsWith("M"))
                .collect(Collectors.toList());
        assertEquals(qList, fList);

        qList = userQuery.where(get(User::getUsername).endsWith("s"))
                .getList();
        fList = userQuery.users().stream()
                .filter(user -> user.getUsername().endsWith("s"))
                .collect(Collectors.toList());
        assertEquals(qList, fList);

        qList = userQuery.where(get(User::getUsername).lower().contains("s"))
                .getList();
        fList = userQuery.users().stream()
                .filter(user -> user.getUsername().toLowerCase().contains("s"))
                .collect(Collectors.toList());
        assertEquals(qList, fList);

        qList = userQuery.where(get(User::getUsername).upper().contains("S"))
                .getList();
        fList = userQuery.users().stream()
                .filter(user -> user.getUsername().toUpperCase().contains("S"))
                .collect(Collectors.toList());
        assertEquals(qList, fList);
    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    public void testResultBuilder(UserEntities userQuery) {
        List<User> resultList = userQuery.orderBy(User::getId).getList(5, 10);
        List<User> subList = userQuery.users().subList(5, 5 + 10);
        assertEquals(resultList, subList);

        List<Integer> userIds = userQuery.select(User::getId)
                .orderBy(User::getId)
                .getList(5, 10);
        List<Integer> subUserIds = userQuery.users().subList(5, 5 + 10)
                .stream().map(User::getId)
                .collect(Collectors.toList());

        assertEquals(userIds, subUserIds);

        resultList = userQuery.where(get(User::getId).in()).getList();
        assertEquals(resultList.size(), 0);

        resultList = userQuery.where(get(User::getId).notIn()).getList();
        assertEquals(resultList, userQuery.users());

        long count = userQuery.count();
        assertEquals(count, userQuery.users().size());

        User first = userQuery.getFirst();
        assertEquals(first, userQuery.users().get(0));

        first = userQuery.where(get(User::getId).eq(0)).requireSingle();
        assertEquals(first, userQuery.users().get(0));

        first = userQuery.getFirst(10);
        assertEquals(first, userQuery.users().get(10));

        assertThrowsExactly(IllegalStateException.class, userQuery::requireSingle);
        assertThrowsExactly(NullPointerException.class, () -> userQuery.where(get(User::getId).eq(-1)).requireSingle());

        assertTrue(userQuery.exist());
        assertTrue(userQuery.exist(userQuery.users().size() - 1));
        assertFalse(userQuery.exist(userQuery.users().size()));

        List<UserModel> userModels = userQuery.select(UserModel.class)
                .orderBy(User::getId)
                .getList();

        List<Map<String, Object>> l0 = userQuery.users().stream()
                .map(UserModel::new)
                .map(UserInterface::asMap)
                .collect(Collectors.toList());

        List<Map<String, Object>> l1 = userQuery.select(UserInterface.class)
                .orderBy(User::getId)
                .getList()
                .stream()
                .map(UserInterface::asMap)
                .collect(Collectors.toList());

        List<Map<String, Object>> l2 = userModels.stream()
                .map(UserInterface::asMap)
                .collect(Collectors.toList());

        assertEquals(l0, l1);
        assertEquals(l0, l2);

        User user = userQuery.users().stream()
                .filter(it -> it.getParentUser() != null)
                .findAny()
                .orElse(userQuery.users().get(0));
        UserInterface userInterface = userQuery.select(UserInterface.class)
                .where(User::getId).eq(user.getId())
                .getSingle();

        assertEquals(userInterface.getId(), user.getId());
        assertEquals(userInterface.getRandomNumber(), user.getRandomNumber());
        assertEquals(userInterface.getUsername(), user.getUsername());
        assertEquals(userInterface.getPid(), user.getPid());
        assertEquals(userInterface.isValid(), user.isValid());
        assertEquals(userInterface.getParentUsername(), user.getParentUser().getUsername());

    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    public void testSlice(UserEntities userQuery) {
        Slice<String> slice = userQuery.select(User::getUsername)
                .where(User::getParentUser).get(User::getRandomNumber).eq(10)
                .groupBy(User::getUsername)
                .slice(2, 10);
        System.out.println(slice);
        long count = userQuery.users().stream()
                .filter(user -> user.getParentUser() != null && user.getParentUser().getRandomNumber() == 10)
                .map(User::getUsername)
                .distinct()
                .count();

        List<String> names = userQuery.users().stream()
                .filter(user -> user.getParentUser() != null && user.getParentUser().getRandomNumber() == 10)
                .map(User::getUsername)
                .distinct()
                .skip(2)
                .limit(10)
                .collect(Collectors.toList());
        assertEquals(slice.total(), count);
        assertEquals(slice.data(), names);
    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    void projection(UserEntities userQuery) throws JsonProcessingException {
        List<UserInterface> list0 = userQuery.select(UserInterface.class)
                .getList();
        List<UserInterface> list1 = userQuery.select(UserInterface.class)
                .getList();

        System.out.println(JsonSerializablePredicateValueTest.mapper.writeValueAsString(list0.get(0)));

        assertEquals(list0, list1);
    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    void testInterfaceSelect(UserEntities userQuery) {
        UserInterface list = userQuery.select(UserInterface.class)
                .getFirst();
        String string = list.toString();
        System.out.println(string);
    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    public void testAttr(UserEntities userQuery) {
        User first = userQuery.orderBy(get(User::getId).desc()).getFirst();
        ArrayList<User> users = new ArrayList<>(userQuery.users());
        users.sort((a, b) -> Integer.compare(b.getId(), a.getId()));
        User f = users.stream().findFirst().orElse(null);
        assertEquals(first, f);

        first = userQuery.orderBy(get(User::getUsername).desc()).getFirst();

        users = new ArrayList<>(userQuery.users());
        users.sort((a, b) -> b.getUsername().compareTo(a.getUsername()));
        f = users.stream().findFirst().orElse(null);
        assertEquals(first, f);

        first = userQuery.orderBy(get(User::isValid).desc()).getFirst();
        users = new ArrayList<>(userQuery.users());
        users.sort((a, b) -> Boolean.compare(b.isValid(), a.isValid()));
        f = users.stream().findFirst().orElse(null);
        assertEquals(first, f);

        first = userQuery
                .where(get(User::isValid).eq(true))
                .getFirst();

        f = userQuery.users().stream()
                .filter(User::isValid)
                .findFirst()
                .orElse(null);
        assertEquals(first, f);

        List<User> resultList = userQuery
                .where(Paths.get(User::getParentUser).get(User::isValid)
                        .eq(true))
                .getList();
        List<User> fList = userQuery.users().stream()
                .filter(user -> user.getParentUser() != null && user.getParentUser().isValid())
                .collect(Collectors.toList());

        assertEquals(resultList, fList);
    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    public void testWhere(UserEntities userQuery) {
        List<User> resultList = userQuery
                .where(Paths.get(User::getParentUser).get(User::getUsername).eq(username))
                .getList();
        List<User> fList = userQuery.users().stream()
                .filter(user -> user.getParentUser() != null && username.equals(user.getParentUser().getUsername()))
                .collect(Collectors.toList());
        assertEquals(resultList, fList);

        resultList = userQuery
                .where(Paths.get(User::getParentUser).get(User::getUsername).ne(username))
                .getList();
        fList = userQuery.users().stream()
                .filter(user -> user.getParentUser() != null && !username.equals(user.getParentUser().getUsername()))
                .collect(Collectors.toList());
        assertEquals(resultList, fList);

        resultList = userQuery
                .where(get(User::getUsername).ne(username))
                .getList();
        fList = userQuery.users().stream()
                .filter(user -> !username.equals(user.getUsername()))
                .collect(Collectors.toList());
        assertEquals(resultList, fList);

        resultList = userQuery
                .where(get(User::getUsername).ne(username))
                .getList();
        fList = userQuery.users().stream()
                .filter(user -> !username.equals(user.getUsername()))
                .collect(Collectors.toList());
        assertEquals(resultList, fList);

        resultList = userQuery
                .where(get(User::getUsername).ne(username))
                .getList();
        fList = userQuery.users().stream()
                .filter(user -> !username.equals(user.getUsername()))
                .collect(Collectors.toList());
        assertEquals(resultList, fList);
    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    public void testPathBuilder(UserEntities userQuery) {
        List<User> resultList = userQuery.where(Paths.get(User::getParentUser)
                        .get(User::getParentUser).get(User::getUsername).eq(username))
                .getList();
        List<User> fList = userQuery.users().stream()
                .filter(user -> {
                    User p = user.getParentUser();
                    return p != null && p.getParentUser() != null && username.equals(p.getParentUser().getUsername());
                })
                .collect(Collectors.toList());
        assertEquals(resultList, fList);

        resultList = userQuery.where(Paths.get(User::getParentUser)
                        .get(User::getRandomNumber).eq(5))
                .getList();
        fList = userQuery.users().stream()
                .filter(user -> {
                    User p = user.getParentUser();
                    return p != null && p.getRandomNumber() == 5;
                })
                .collect(Collectors.toList());
        assertEquals(resultList, fList);

        resultList = userQuery.where(Paths.get(User::getParentUser)
                        .get(User::getRandomNumber).eq(5))
                .getList();
        fList = userQuery.users().stream()
                .filter(user -> {
                    User p = user.getParentUser();
                    return p != null && p.getRandomNumber() == 5;
                })
                .collect(Collectors.toList());
        assertEquals(resultList, fList);
    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    public void testBigNum(UserEntities userQuery) {
        List<User> users = userQuery.where(get(User::getTimestamp).eq(Double.MAX_VALUE))
                .getList();
        System.out.println(users);
    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    public void subQueryTest(UserEntities userQuery) {
        Expression<User, List<Integer>> ids = userQuery
                .select(User::getId).where(User::getId)
                .in(1, 2, 3)
                .asSubQuery();

        List<User> result = userQuery.where(User::getId).in(ids).getList();
        System.out.println(result);
    }

    private IntStream getUserIdStream(UserEntities userQuery) {
        return userQuery.users().stream().mapToInt(User::getRandomNumber);
    }

    // @Test
    // void test() {
    //     test(UserQueryProvider.jpaQuery());
    //     test(UserQueryProvider.jdbcQuery());
    // }

    // private static void test(Query query) {
    //     Select<UserSummary> from = query.from(UserSummary.class);
    //     List<UserSummary> list = from.where(UserSummary::getMaxRandomNumber).le(33).getList();
    //     Map<String, List<User>> map = userQuery.users().stream().collect(Collectors.groupingBy(User::getUsername));
    //     Map<String, UserSummary> summaryMap = new HashMap<>();
    //     map.forEach((k, v) -> {
    //         UserSummary summary = new UserSummary();
    //         summary.setCount((long) v.size());
    //         summary.setUsername(k);
    //         int maxRandomNumber = Integer.MIN_VALUE;
    //         for (User user : v) {
    //             maxRandomNumber = Math.max(maxRandomNumber, user.getRandomNumber());
    //         }
    //         summary.setMaxRandomNumber(maxRandomNumber);
    //         summaryMap.put(k, summary);
    //     });
    //     for (UserSummary summary : list) {
    //         UserSummary s = summaryMap.get(summary.getUsername());
    //         assertEquals(s, summary);
    //         assertTrue(summary.getMaxRandomNumber() <= 33);
    //     }
    // }
}
