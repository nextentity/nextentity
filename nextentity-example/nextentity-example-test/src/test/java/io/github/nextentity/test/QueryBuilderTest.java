package io.github.nextentity.test;

import io.github.nextentity.api.*;
import io.github.nextentity.api.TypedExpression.Predicate;
import io.github.nextentity.api.model.EntityRoot;
import io.github.nextentity.api.model.LockModeType;
import io.github.nextentity.api.model.Slice;
import io.github.nextentity.api.model.Tuple;
import io.github.nextentity.api.model.Tuple2;
import io.github.nextentity.core.Tuples;
import io.github.nextentity.core.expression.Expressions;
import io.github.nextentity.core.util.ImmutableList;
import io.github.nextentity.core.util.Paths;
import io.github.nextentity.test.db.UserRepository;
import io.github.nextentity.test.domain.Page;
import io.github.nextentity.test.domain.Pageable;
import io.github.nextentity.test.entity.User;
import io.github.nextentity.test.projection.IUser;
import io.github.nextentity.test.projection.UserInterface;
import io.github.nextentity.test.projection.UserModel;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ArgumentsSource;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static io.github.nextentity.core.util.Paths.get;
import static org.junit.jupiter.api.Assertions.*;

@Slf4j
class QueryBuilderTest {

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    void select2(UserRepository userQuery) {
        List<Tuple2<Integer, Integer>> list = userQuery.selectDistinct(
                        User::getId, User::getRandomNumber)
                // .orderBy(User::getId)
                .getList(10, 20);
        System.out.println(list);
    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    void select3(UserRepository userQuery) {
        IUser first1 = userQuery.select(IUser.class).getFirst(90);
        System.out.println(first1);
    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    void select4(UserRepository userQuery) {
        List<User> list = userQuery.selectDistinct(User::getParentUser).getList();
        System.out.println(list);
    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    void select(UserRepository userQuery) {

        int offset = 90;
        User f2 = userQuery.fetch(User::getParentUser).orderBy(User::getId).getFirst(offset);
        IUser first1 = userQuery.select(IUser.class).orderBy(User::getId).getFirst(offset);
        Assertions.assertEquals(first1.getUsername(), f2.getUsername());
        Assertions.assertEquals(first1.getId(), f2.getId());
        Assertions.assertEquals(first1.getRandomNumber(), f2.getRandomNumber());
        if (f2.getParentUser() != null) {
            Assertions.assertEquals(first1.getParentUser().getUsername(), f2.getParentUser().getUsername());
            Assertions.assertEquals(first1.getParentUser().getId(), f2.getParentUser().getId());
            Assertions.assertEquals(first1.getParentUser().getRandomNumber(), f2.getParentUser().getRandomNumber());
        }

        User first3 = userQuery.fetch(User::getParentUser).getFirst(offset);
        System.out.println(first3);
        System.out.println(first3.getParentUser());
        IUser.U first2 = userQuery.select(IUser.U.class).getFirst(offset);
        Assertions.assertEquals(first2.getUsername(), f2.getUsername());
        Assertions.assertEquals(first2.getId(), f2.getId());
        Assertions.assertEquals(first2.getRandomNumber(), f2.getRandomNumber());

        User first = userQuery.select(User.class).getFirst();
        assertEquals(first, userQuery.users().get(0));

        Integer firstUserid = userQuery.select(User::getId).getFirst();
        assertEquals(firstUserid, userQuery.users().get(0).getId());

        Tuple2<Integer, Integer> array = userQuery.select(User::getId, User::getRandomNumber).getFirst();
        assertEquals(array.get0(), userQuery.users().get(0).getId());
        assertEquals(array.get1(), userQuery.users().get(0).getRandomNumber());

        UserModel model = userQuery.select(UserModel.class).getFirst();
        assertEquals(model, new UserModel(userQuery.users().get(0)));

        UserInterface ui = userQuery.select(UserInterface.class).getFirst();
        assertEquals(model.asMap(), ui.asMap());

        ui = userQuery.selectDistinct(UserInterface.class).orderBy(User::getId).getFirst();
        assertEquals(model.asMap(), ui.asMap());

        Long count = userQuery.select(get(User::getId).count()).getSingle();
        assertEquals(count, userQuery.users().size());

        Tuple aggArray = userQuery.select(ImmutableList.of(
                get(User::getId).count(),
                get(User::getRandomNumber).max(),
                get(User::getRandomNumber).min(),
                get(User::getRandomNumber).sum(),
                get(User::getRandomNumber).avg()
        )).getSingle();

        int max = Integer.MIN_VALUE;
        int min = Integer.MAX_VALUE;
        int sum = 0;
        for (User user : userQuery.users()) {
            int number = user.getRandomNumber();
            max = Math.max(max, number);
            min = Math.min(min, number);
            sum += number;
        }

        assertEquals(aggArray.<Long>get(0), userQuery.users().size());
        assertEquals(aggArray.<Integer>get(1), max);
        assertEquals(aggArray.<Integer>get(2), min);
        assertEquals(aggArray.<Number>get(3).intValue(), sum);
        assertEquals(aggArray.<Number>get(4).doubleValue(), sum * 1.0 / userQuery.users().size(), 1);


        assertEquals(
                userQuery
                        .select(User::getId, User::getRandomNumber)
                        .getList(),

                userQuery.users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber()))
                        .collect(Collectors.toList())
        );

        assertEquals(
                userQuery
                        .select(User::getId, User::getRandomNumber, User::getTime)
                        .getList(),

                userQuery.users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber(), it.getTime()))
                        .collect(Collectors.toList())
        );

        assertEquals(
                userQuery
                        .select(User::getId, User::getRandomNumber, User::getTime, User::getPid)
                        .getList(),

                userQuery.users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber(), it.getTime(), it.getPid()))
                        .collect(Collectors.toList())
        );


        assertEquals(
                userQuery
                        .select(User::getId, User::getRandomNumber, User::getTime, User::getPid,
                                User::getTimestamp)
                        .getList(),

                userQuery.users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber(), it.getTime(), it.getPid(),
                                it.getTimestamp()))
                        .collect(Collectors.toList())
        );

        assertEquals(
                userQuery
                        .select(User::getId, User::getRandomNumber, User::getTime, User::getPid,
                                User::getTimestamp, User::isValid)
                        .getList(),

                userQuery.users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber(), it.getTime(), it.getPid(),
                                it.getTimestamp(), it.isValid()))
                        .collect(Collectors.toList())
        );

        assertEquals(
                userQuery
                        .select(User::getId, User::getRandomNumber, User::getTime, User::getPid,
                                User::getTimestamp, User::isValid, User::getGender)
                        .getList(),

                userQuery.users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber(), it.getTime(), it.getPid(),
                                it.getTimestamp(), it.isValid(), it.getGender()))
                        .collect(Collectors.toList())
        );

        assertEquals(
                userQuery
                        .select(User::getId, User::getRandomNumber, User::getTime, User::getPid,
                                User::getTimestamp, User::isValid, User::getGender, User::getInstant)
                        .getList(),

                userQuery.users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber(), it.getTime(), it.getPid(),
                                it.getTimestamp(), it.isValid(), it.getGender(), it.getInstant()))
                        .collect(Collectors.toList())
        );

        assertEquals(
                userQuery
                        .select(User::getId, User::getRandomNumber, User::getTime, User::getPid,
                                User::getTimestamp, User::isValid, User::getGender, User::getInstant,
                                User::getTestLong)
                        .getList(),

                userQuery.users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber(), it.getTime(), it.getPid(),
                                it.getTimestamp(), it.isValid(), it.getGender(), it.getInstant(),
                                it.getTestLong()))
                        .collect(Collectors.toList())
        );

        assertEquals(
                userQuery
                        .select(User::getId, User::getRandomNumber, User::getTime, User::getPid,
                                User::getTimestamp, User::isValid, User::getGender, User::getInstant,
                                User::getTestLong, User::getTestInteger)
                        .getList(),

                userQuery.users().stream()
                        .map(user -> Tuples.of(
                                user.getId(), user.getRandomNumber(), user.getTime(), user.getPid(),
                                user.getTimestamp(), user.isValid(), user.getGender(), user.getInstant(),
                                user.getTestLong(), user.getTestInteger()
                        ))
                        .collect(Collectors.toList())
        );


        assertEquals(
                userQuery
                        .select(ImmutableList.<Path<User, ?>>of(User::getId, User::getRandomNumber, User::getTime, User::getPid,
                                User::getTimestamp, User::isValid, User::getGender, User::getInstant,
                                User::getTestLong, User::getTestInteger))
                        .getList(),

                userQuery.users().stream()
                        .map(user -> Tuples.of(
                                user.getId(), user.getRandomNumber(), user.getTime(), user.getPid(),
                                user.getTimestamp(), user.isValid(), user.getGender(), user.getInstant(),
                                user.getTestLong(), user.getTestInteger()
                        ))
                        .collect(Collectors.toList())
        );

        assertEquals(
                userQuery
                        .select(User::getId, User::getRandomNumber, User::getTime, User::getPid,
                                User::getTimestamp, User::isValid, User::getGender, User::getInstant,
                                User::getTestLong, User::getTestInteger)
                        .getList(),

                userQuery.users().stream()
                        .map(user -> Tuples.of(
                                user.getId(), user.getRandomNumber(), user.getTime(), user.getPid(),
                                user.getTimestamp(), user.isValid(), user.getGender(), user.getInstant(),
                                user.getTestLong(), user.getTestInteger()
                        ))
                        .collect(Collectors.toList())
        );


        assertDistinctEquals(
                userQuery
                        .selectDistinct(User::getTestLocalDate, User::getRandomNumber)
                        .getList(),

                userQuery.users().stream()
                        .map(it -> Tuples.of(it.getTestLocalDate(), it.getRandomNumber()))
                        .distinct()
                        .collect(Collectors.toList())
        );

        assertDistinctEquals(
                userQuery
                        .selectDistinct(User::getTestLocalDate, User::getRandomNumber, User::getTime)
                        .getList(),

                userQuery.users().stream()
                        .map(it -> Tuples.of(it.getTestLocalDate(), it.getRandomNumber(), it.getTime()))
                        .distinct()
                        .collect(Collectors.toList())
        );

        assertDistinctEquals(
                userQuery
                        .selectDistinct(User::getTestLocalDate, User::getRandomNumber, User::getTime, User::getPid)
                        .getList(),

                userQuery.users().stream()
                        .map(it -> Tuples.of(it.getTestLocalDate(), it.getRandomNumber(), it.getTime(), it.getPid()))
                        .distinct()
                        .collect(Collectors.toList())
        );


        assertDistinctEquals(
                userQuery
                        .selectDistinct(User::getTestLocalDate, User::getRandomNumber, User::getTime, User::getPid,
                                User::getTimestamp)
                        .getList(),

                userQuery.users().stream()
                        .map(it -> Tuples.of(it.getTestLocalDate(), it.getRandomNumber(), it.getTime(), it.getPid(),
                                it.getTimestamp()))
                        .distinct()
                        .collect(Collectors.toList())
        );

        assertDistinctEquals(
                userQuery
                        .selectDistinct(User::getTestLocalDate, User::getRandomNumber, User::getTime, User::getPid,
                                User::getTimestamp, User::isValid)
                        .getList(),

                userQuery.users().stream()
                        .map(it -> Tuples.of(it.getTestLocalDate(), it.getRandomNumber(), it.getTime(), it.getPid(),
                                it.getTimestamp(), it.isValid()))
                        .distinct()
                        .collect(Collectors.toList())
        );

        assertDistinctEquals(
                userQuery
                        .selectDistinct(User::getTestLocalDate, User::getRandomNumber, User::getTime, User::getPid,
                                User::getTimestamp, User::isValid, User::getGender)
                        .getList(),

                userQuery.users().stream()
                        .map(it -> Tuples.of(it.getTestLocalDate(), it.getRandomNumber(), it.getTime(), it.getPid(),
                                it.getTimestamp(), it.isValid(), it.getGender()))
                        .distinct()
                        .collect(Collectors.toList())
        );

        assertDistinctEquals(
                userQuery
                        .selectDistinct(User::getTestLocalDate, User::getRandomNumber, User::getTime, User::getPid,
                                User::getTimestamp, User::isValid, User::getGender, User::getInstant)
                        .getList(),

                userQuery.users().stream()
                        .map(it -> Tuples.of(it.getTestLocalDate(), it.getRandomNumber(), it.getTime(), it.getPid(),
                                it.getTimestamp(), it.isValid(), it.getGender(), it.getInstant()))
                        .distinct()
                        .collect(Collectors.toList())
        );

        assertDistinctEquals(
                userQuery
                        .selectDistinct(User::getTestLocalDate, User::getRandomNumber, User::getTime, User::getPid,
                                User::getTimestamp, User::isValid, User::getGender, User::getInstant,
                                User::getTestLong)
                        .getList(),

                userQuery.users().stream()
                        .map(it -> Tuples.of(it.getTestLocalDate(), it.getRandomNumber(), it.getTime(), it.getPid(),
                                it.getTimestamp(), it.isValid(), it.getGender(), it.getInstant(),
                                it.getTestLong()))
                        .distinct()
                        .collect(Collectors.toList())
        );

        assertDistinctEquals(
                userQuery
                        .selectDistinct(User::getTestLocalDate, User::getRandomNumber, User::getTime, User::getPid,
                                User::getTimestamp, User::isValid, User::getGender, User::getInstant,
                                User::getTestLong, User::getTestInteger)
                        .getList(),

                userQuery.users().stream()
                        .map(user -> Tuples.of(
                                user.getTestLocalDate(), user.getRandomNumber(), user.getTime(), user.getPid(),
                                user.getTimestamp(), user.isValid(), user.getGender(), user.getInstant(),
                                user.getTestLong(), user.getTestInteger()
                        ))
                        .distinct()
                        .collect(Collectors.toList())
        );

        assertDistinctEquals(
                userQuery
                        .selectDistinct(User::getTestLocalDate, User::getRandomNumber, User::getTime, User::getPid,
                                User::getTimestamp, User::isValid, User::getGender, User::getInstant,
                                User::getTestLong, User::getTestInteger)
                        .getList(),

                userQuery.users().stream()
                        .map(user -> Tuples.of(
                                user.getTestLocalDate(), user.getRandomNumber(), user.getTime(), user.getPid(),
                                user.getTimestamp(), user.isValid(), user.getGender(), user.getInstant(),
                                user.getTestLong(), user.getTestInteger()
                        ))
                        .distinct()
                        .collect(Collectors.toList())
        );


        assertEquals(
                userQuery
                        .select(User::getId, User::getRandomNumber)
                        .groupBy(User::getId, User::getRandomNumber)
                        .getList(),

                userQuery.users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber()))
                        .distinct()
                        .collect(Collectors.toList())
        );

        assertEquals(
                userQuery
                        .select(User::getId, User::getRandomNumber, User::getTime)
                        .groupBy(User::getId, User::getRandomNumber, User::getTime)
                        .getList(),

                userQuery.users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber(), it.getTime()))
                        .distinct()
                        .collect(Collectors.toList())
        );

        assertEquals(
                userQuery
                        .select(User::getId, User::getRandomNumber, User::getTime, User::getPid)
                        .groupBy(User::getId, User::getRandomNumber, User::getTime, User::getPid)
                        .getList(),

                userQuery.users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber(), it.getTime(), it.getPid()))
                        .distinct()
                        .collect(Collectors.toList())
        );


        assertEquals(
                userQuery
                        .select(User::getId, User::getRandomNumber, User::getTime, User::getPid,
                                User::getTimestamp)
                        .groupBy(User::getId, User::getRandomNumber, User::getTime, User::getPid,
                                User::getTimestamp)
                        .getList(),

                userQuery.users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber(), it.getTime(), it.getPid(),
                                it.getTimestamp()))
                        .distinct()
                        .collect(Collectors.toList())
        );

        assertEquals(
                userQuery
                        .select(User::getId, User::getRandomNumber, User::getTime, User::getPid,
                                User::getTimestamp, User::isValid)
                        .groupBy(User::getId, User::getRandomNumber, User::getTime, User::getPid,
                                User::getTimestamp, User::isValid)
                        .getList(),

                userQuery.users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber(), it.getTime(), it.getPid(),
                                it.getTimestamp(), it.isValid()))
                        .distinct()
                        .collect(Collectors.toList())
        );


        assertEquals(
                userQuery
                        .select(get(User::getId), get(User::getRandomNumber))
                        .getList(),

                userQuery.users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber()))
                        .collect(Collectors.toList())
        );

        assertEquals(
                userQuery
                        .select(get(User::getId), get(User::getRandomNumber), get(User::getTime))
                        .getList(),

                userQuery.users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber(), it.getTime()))
                        .collect(Collectors.toList())
        );

        assertEquals(
                userQuery
                        .select(get(User::getId), get(User::getRandomNumber), get(User::getTime), get(User::getPid))
                        .getList(),

                userQuery.users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber(), it.getTime(), it.getPid()))
                        .collect(Collectors.toList())
        );


        assertEquals(
                userQuery
                        .select(get(User::getId), get(User::getRandomNumber), get(User::getTime), get(User::getPid),
                                get(User::getTimestamp))
                        .getList(),

                userQuery.users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber(), it.getTime(), it.getPid(),
                                it.getTimestamp()))
                        .collect(Collectors.toList())
        );

        assertEquals(
                userQuery
                        .select(get(User::getId), get(User::getRandomNumber), get(User::getTime), get(User::getPid),
                                get(User::getTimestamp), get(User::isValid))
                        .getList(),

                userQuery.users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber(), it.getTime(), it.getPid(),
                                it.getTimestamp(), it.isValid()))
                        .collect(Collectors.toList())
        );

        assertEquals(
                userQuery
                        .select(get(User::getId), get(User::getRandomNumber), get(User::getTime), get(User::getPid),
                                get(User::getTimestamp), get(User::isValid), get(User::getGender))
                        .getList(),

                userQuery.users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber(), it.getTime(), it.getPid(),
                                it.getTimestamp(), it.isValid(), it.getGender()))
                        .collect(Collectors.toList())
        );

        assertEquals(
                userQuery
                        .select(get(User::getId), get(User::getRandomNumber), get(User::getTime), get(User::getPid),
                                get(User::getTimestamp), get(User::isValid), get(User::getGender), get(User::getInstant))
                        .getList(),

                userQuery.users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber(), it.getTime(), it.getPid(),
                                it.getTimestamp(), it.isValid(), it.getGender(), it.getInstant()))
                        .collect(Collectors.toList())
        );

        assertEquals(
                userQuery
                        .select(get(User::getId), get(User::getRandomNumber), get(User::getTime), get(User::getPid),
                                get(User::getTimestamp), get(User::isValid), get(User::getGender), get(User::getInstant),
                                get(User::getTestLong))
                        .getList(),

                userQuery.users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber(), it.getTime(), it.getPid(),
                                it.getTimestamp(), it.isValid(), it.getGender(), it.getInstant(),
                                it.getTestLong()))
                        .collect(Collectors.toList())
        );

        assertEquals(
                userQuery
                        .select(get(User::getId), get(User::getRandomNumber), get(User::getTime), get(User::getPid),
                                get(User::getTimestamp), get(User::isValid), get(User::getGender), get(User::getInstant),
                                get(User::getTestLong), get(User::getTestInteger))
                        .getList(),

                userQuery.users().stream()
                        .map(user -> Tuples.of(
                                user.getId(), user.getRandomNumber(), user.getTime(), user.getPid(),
                                user.getTimestamp(), user.isValid(), user.getGender(), user.getInstant(),
                                user.getTestLong(), user.getTestInteger()
                        ))
                        .collect(Collectors.toList())
        );

        assertEquals(
                userQuery
                        .select(get(User::getId), get(User::getRandomNumber), get(User::getTime), get(User::getPid),
                                get(User::getTimestamp), get(User::isValid), get(User::getGender), get(User::getInstant),
                                get(User::getTestLong), get(User::getTestInteger))
                        .getList(),

                userQuery.users().stream()
                        .map(user -> Tuples.of(
                                user.getId(), user.getRandomNumber(), user.getTime(), user.getPid(),
                                user.getTimestamp(), user.isValid(), user.getGender(), user.getInstant(),
                                user.getTestLong(), user.getTestInteger()
                        ))
                        .collect(Collectors.toList())
        );


        assertDistinctEquals(
                userQuery
                        .selectDistinct(get(User::getId), get(User::getRandomNumber))
                        .getList(),

                userQuery.users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber()))
                        .distinct()
                        .collect(Collectors.toList())
        );

        assertDistinctEquals(
                userQuery
                        .selectDistinct(get(User::getId), get(User::getRandomNumber), get(User::getTime))
                        .getList(),

                userQuery.users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber(), it.getTime()))
                        .collect(Collectors.toList())
        );

        assertDistinctEquals(
                userQuery
                        .selectDistinct(get(User::getId), get(User::getRandomNumber), get(User::getTime), get(User::getPid))
                        .getList(),

                userQuery.users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber(), it.getTime(), it.getPid()))
                        .collect(Collectors.toList())
        );


        assertDistinctEquals(
                userQuery
                        .selectDistinct(get(User::getId), get(User::getRandomNumber), get(User::getTime), get(User::getPid),
                                get(User::getTimestamp))
                        .getList(),

                userQuery.users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber(), it.getTime(), it.getPid(),
                                it.getTimestamp()))
                        .collect(Collectors.toList())
        );

        assertDistinctEquals(
                userQuery
                        .selectDistinct(get(User::getId), get(User::getRandomNumber), get(User::getTime), get(User::getPid),
                                get(User::getTimestamp), get(User::isValid))
                        .getList(),

                userQuery.users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber(), it.getTime(), it.getPid(),
                                it.getTimestamp(), it.isValid()))
                        .collect(Collectors.toList())
        );

        assertDistinctEquals(
                userQuery
                        .selectDistinct(get(User::getId), get(User::getRandomNumber), get(User::getTime), get(User::getPid),
                                get(User::getTimestamp), get(User::isValid), get(User::getGender))
                        .getList(),

                userQuery.users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber(), it.getTime(), it.getPid(),
                                it.getTimestamp(), it.isValid(), it.getGender()))
                        .collect(Collectors.toList())
        );

        assertDistinctEquals(
                userQuery
                        .selectDistinct(get(User::getId), get(User::getRandomNumber), get(User::getTime), get(User::getPid),
                                get(User::getTimestamp), get(User::isValid), get(User::getGender), get(User::getInstant))
                        .getList(),

                userQuery.users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber(), it.getTime(), it.getPid(),
                                it.getTimestamp(), it.isValid(), it.getGender(), it.getInstant()))
                        .collect(Collectors.toList())
        );

        assertDistinctEquals(
                userQuery
                        .selectDistinct(get(User::getId), get(User::getRandomNumber), get(User::getTime), get(User::getPid),
                                get(User::getTimestamp), get(User::isValid), get(User::getGender), get(User::getInstant),
                                get(User::getTestLong))
                        .getList(),

                userQuery.users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber(), it.getTime(), it.getPid(),
                                it.getTimestamp(), it.isValid(), it.getGender(), it.getInstant(),
                                it.getTestLong()))
                        .collect(Collectors.toList())
        );

        assertDistinctEquals(
                userQuery
                        .selectDistinct(get(User::getId), get(User::getRandomNumber), get(User::getTime), get(User::getPid),
                                get(User::getTimestamp), get(User::isValid), get(User::getGender), get(User::getInstant),
                                get(User::getTestLong), get(User::getTestInteger))
                        .getList(),

                userQuery.users().stream()
                        .map(user -> Tuples.of(
                                user.getId(), user.getRandomNumber(), user.getTime(), user.getPid(),
                                user.getTimestamp(), user.isValid(), user.getGender(), user.getInstant(),
                                user.getTestLong(), user.getTestInteger()
                        ))
                        .collect(Collectors.toList())
        );

        assertDistinctEquals(
                userQuery
                        .selectDistinct(get(User::getId), get(User::getRandomNumber), get(User::getTime), get(User::getPid),
                                get(User::getTimestamp), get(User::isValid), get(User::getGender), get(User::getInstant),
                                get(User::getTestLong), get(User::getTestInteger))
                        .getList(),

                userQuery.users().stream()
                        .map(user -> Tuples.of(
                                user.getId(), user.getRandomNumber(), user.getTime(), user.getPid(),
                                user.getTimestamp(), user.isValid(), user.getGender(), user.getInstant(),
                                user.getTestLong(), user.getTestInteger()
                        ))
                        .collect(Collectors.toList())
        );


    }

    private <T> void assertDistinctEquals(List<T> list, List<T> collect) {
        assertEquals(list.size(), collect.size());
        Set<T> a = new HashSet<>(list);
        Set<T> b = new HashSet<>(collect);
        assertEquals(a.size(), list.size());
        assertEquals(a, b);
    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    void selectDistinct(UserRepository userQuery) {
        List<Integer> list = userQuery.selectDistinct(User::getRandomNumber)
                .getList();
        List<Integer> collect = userQuery.users()
                .stream().map(User::getRandomNumber)
                .distinct()
                .collect(Collectors.toList());
        assertDistinctEquals(list, collect);


        list = userQuery.selectDistinct(get(User::getRandomNumber))
                .getList();
        assertDistinctEquals(list, collect);

        List<Tuple> tuples = userQuery.selectDistinct(ImmutableList.<Path<User, ?>>of(User::getRandomNumber, User::getUsername))
                .getList();

        List<Tuple> collect2 = userQuery.users().stream()
                .map(user -> Tuples.of(user.getRandomNumber(), user.getUsername()))
                .distinct()
                .collect(Collectors.toList());

        assertDistinctEquals(tuples, collect2);

    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    void fetch(UserRepository userQuery) {
        List<User> users = userQuery.fetch(User::getParentUser).orderBy(User::getId).getList();

        assertEquals(users, userQuery.users());
        for (int i = 0; i < userQuery.users().size(); i++) {
            User a = users.get(i);
            User b = userQuery.users().get(i);
            if (b.getParentUser() != null) {
                assertEquals(b.getParentUser(), a.getParentUser());
            } else {
                assertNull(a.getParentUser());
            }
        }

        users = userQuery.fetch(Paths.get(User::getParentUser).get(User::getParentUser))
                .orderBy(User::getId)
                .getList();

        assertEquals(users, userQuery.users());
        for (int i = 0; i < userQuery.users().size(); i++) {
            User a = users.get(i);
            User b = userQuery.users().get(i);
            if (b.getParentUser() != null) {
                b = b.getParentUser();
                a = a.getParentUser();
                assertNotNull(a);
                if (b.getParentUser() != null) {
                    assertEquals(b.getParentUser(), a.getParentUser());
                }
            }
        }

    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    void groupBy(UserRepository userQuery) {
        List<Tuple> list = userQuery
                .select(ImmutableList.of(
                        get(User::getRandomNumber),
                        get(User::getId).count()
                ))
                .groupBy(User::getRandomNumber)
                .getList();

        Map<Object, Long> count = userQuery.users().stream()
                .collect(Collectors.groupingBy(User::getRandomNumber, Collectors.counting()));

        assertEquals(list.size(), count.size());
        for (Tuple objects : list) {
            Long value = count.get(objects.get(0));
            assertEquals(value, objects.get(1));
        }

        list = userQuery
                .select(ImmutableList.of(
                        get(User::getRandomNumber),
                        get(User::getId).count()
                ))
                .groupBy(ImmutableList.<Path<User, ?>>of(User::getRandomNumber))
                .getList();

        assertEquals(list.size(), count.size());
        for (Tuple objects : list) {
            Long value = count.get(objects.get(0));
            assertEquals(value, objects.get(1));
        }


    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    void orderBy(UserRepository userQuery) {
        testOrderBy(ImmutableList.of(new Checker<>(userQuery.users(), userQuery)));
    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    void combinatorial(UserRepository userQuery) {
        testOrderBy(getWhereTestCase(new Checker<>(userQuery.users(), userQuery), userQuery.users()));

    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    void testEmptyIn(UserRepository userQuery) {
        List<User> list = userQuery.where(User::getId).notIn().getList();
        assertEquals(list, userQuery.users());
    }

    private static void testOrderBy(List<Checker<User, SelectOrderByStep<User, User>>> testcase) {
        for (Checker<User, SelectOrderByStep<User, User>> checker : testcase) {
            ArrayList<User> sorted = new ArrayList<>(checker.expected);
            sorted.sort(Comparator.comparingInt(User::getRandomNumber));
            List<User> users = checker.collector
                    .orderBy(User::getRandomNumber, User::getId).asc()
                    .getList();
            assertEquals(users, sorted);

            assertEquals(checker.collector
                    .orderBy(User::getRandomNumber, User::getId)
                    .count(), sorted.size());

            assertEquals(checker.collector
                    .orderBy(User::getRandomNumber, User::getId)
                    .exist(0), !sorted.isEmpty());

            Slice<User> slice = checker.collector
                    .orderBy(User::getRandomNumber, User::getId)
                    .slice(0, 1);
            assertEquals(slice.total(), sorted.size());
            EntityRoot<User> root = checker.collector
                    .orderBy(User::getRandomNumber, User::getId).root();
            assertEquals(root, Paths.root());

            users = checker.collector
                    .orderBy((EntityRoot<User> r) -> ImmutableList.of(
                            r.get(User::getRandomNumber).asc(),
                            r.get(User::getId).asc()
                    ))
                    .getList();
            assertEquals(users, sorted);
            users = checker.collector.orderBy(User::getRandomNumber, User::getId)
                    .getList();
            assertEquals(users, sorted);
            users = checker.collector.orderBy(User::getRandomNumber, User::getId).desc()
                    .getList();
            sorted = new ArrayList<>(checker.expected);
            sorted.sort((a, b) -> {
                int compare = Integer.compare(b.getRandomNumber(), a.getRandomNumber());
                if (compare == 0) {
                    compare = Integer.compare(b.getId(), a.getId());
                }
                return compare;
            });
            assertEquals(users, sorted);

            users = checker.collector.orderBy(User::getRandomNumber).desc()
                    .orderBy(User::getId)
                    .getList();
            sorted = new ArrayList<>(checker.expected);
            sorted.sort((a, b) -> {
                int compare = Integer.compare(b.getRandomNumber(), a.getRandomNumber());
                if (compare == 0) {
                    compare = Integer.compare(a.getId(), b.getId());
                }
                return compare;
            });
            assertEquals(users, sorted);
        }
    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    void getList(UserRepository userQuery) {

        List<User> users = userQuery.getList();
        assertEquals(users.size(), userQuery.users().size());

        users = userQuery.getList(0, 10);
        assertEquals(users, userQuery.users().subList(0, 10));

        users = userQuery.getList(100, 15);
        assertEquals(users, userQuery.users().subList(100, 115));

    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    void where(UserRepository userQuery) {
        Checker<User, SelectWhereStep<User, User>> check = new Checker<>(userQuery.users(), userQuery);
        getWhereTestCase(check, userQuery.users());
    }

    private List<Checker<User, SelectOrderByStep<User, User>>> whereTestCase;


    private List<Checker<User, SelectOrderByStep<User, User>>> getWhereTestCase(Checker<User, SelectWhereStep<User, User>> check, List<User> users) {
        if (whereTestCase != null) {
            return whereTestCase;
        }
        List<Checker<User, SelectOrderByStep<User, User>>> result = whereTestCase = new ArrayList<>();
        String username = users.get(10).getUsername();

        SelectWhereStep<User, User> userQuery = check.collector;
        SelectOrderByStep<User, User> collector = userQuery
                .where(Paths.get(User::getParentUser).get(User::getUsername).eq(username));
        Stream<User> stream = newStream(check)
                .filter(user -> user.getParentUser() != null && username.equals(user.getParentUser().getUsername()));
        result.add(new Checker<>(stream, collector));

        stream = newStream(check)
                .filter(user -> user.getParentUser() != null && !username.equals(user.getParentUser().getUsername()));
        collector = userQuery
                .where(Paths.get(User::getParentUser).get(User::getUsername).ne(username));
        result.add(new Checker<>(stream, collector));

        collector = userQuery
                .where(get(User::getUsername).ne(username));
        stream = newStream(check)
                .filter(user -> !username.equals(user.getUsername()));
        result.add(new Checker<>(stream, collector));

        collector = userQuery
                .where(get(User::getUsername).ne(username));
        stream = newStream(check)
                .filter(user -> !username.equals(user.getUsername()));
        result.add(new Checker<>(stream, collector));

        collector = userQuery
                .where(get(User::getUsername).ne(username));
        stream = newStream(check)
                .filter(user -> !username.equals(user.getUsername()));
        result.add(new Checker<>(stream, collector));


        Predicate<User> isValid = get(User::isValid);
        collector = userQuery.where(isValid);
        stream = users.stream().filter(User::isValid);

        result.add(new Checker<>(stream, collector));

        collector = userQuery.where(isValid.and(User::getRandomNumber).eq(2));
        stream = newStream(check).filter(User::isValid).filter(user -> user.getRandomNumber() == 2);
        result.add(new Checker<>(stream, collector));

        collector = userQuery.where(isValid.and(User::getPid).ne(2));
        stream = newStream(check).filter(User::isValid).filter(user -> user.getPid() != null && user.getPid() != 2);
        result.add(new Checker<>(stream, collector));

        collector = userQuery.where(isValid.and(User::getRandomNumber).in(1, 2, 3));
        stream = newStream(check).filter(User::isValid).filter(user -> Arrays.asList(1, 2, 3).contains(user.getRandomNumber()));
        result.add(new Checker<>(stream, collector));

        collector = userQuery.where(isValid.and(User::getRandomNumber).notIn(1, 2, 3));
        stream = newStream(check).filter(User::isValid).filter(user -> !Arrays.asList(1, 2, 3).contains(user.getRandomNumber()));
        result.add(new Checker<>(stream, collector));

        collector = userQuery.where(isValid.and(User::getPid).isNull());
        stream = newStream(check).filter(User::isValid).filter(user -> user.getPid() == null);
        result.add(new Checker<>(stream, collector));

        collector = userQuery.where(isValid.and(User::getRandomNumber).ge(10));
        stream = newStream(check).filter(User::isValid).filter(user -> user.getRandomNumber() >= 10);
        result.add(new Checker<>(stream, collector));

        collector = userQuery.where(isValid.and(User::getRandomNumber).gt(10));
        stream = newStream(check).filter(User::isValid).filter(user -> user.getRandomNumber() > 10);
        result.add(new Checker<>(stream, collector));

        collector = userQuery.where(isValid.and(User::getRandomNumber).le(10));
        stream = newStream(check).filter(User::isValid).filter(user -> user.getRandomNumber() <= 10);
        result.add(new Checker<>(stream, collector));

        collector = userQuery.where(isValid.and(User::getRandomNumber).lt(10));
        stream = newStream(check).filter(User::isValid).filter(user -> user.getRandomNumber() < 10);
        result.add(new Checker<>(stream, collector));

        collector = userQuery.where(isValid.and(User::getRandomNumber).between(10, 15));
        stream = newStream(check).filter(User::isValid).filter(user -> user.getRandomNumber() >= 10 && user.getRandomNumber() <= 15);
        result.add(new Checker<>(stream, collector));

        collector = userQuery.where(isValid.and(User::getRandomNumber).notBetween(10, 15));
        stream = newStream(check).filter(User::isValid).filter(user -> user.getRandomNumber() < 10 || user.getRandomNumber() > 15);
        result.add(new Checker<>(stream, collector));

        collector = userQuery.where(isValid
                .and(User::getRandomNumber).notBetween(10, 15)
                .and(User::getId).mod(3).eq(0)
        );
        stream = newStream(check).filter(User::isValid).filter(user ->
                !(user.getRandomNumber() >= 10 && user.getRandomNumber() <= 15)
                && user.getId() % 3 == 0);
        result.add(new Checker<>(stream, collector));

        collector = userQuery.where(isValid.and(User::getRandomNumber).ge(get(User::getPid)));
        stream = newStream(check).filter(User::isValid)
                .filter(user -> user.getPid() != null && user.getRandomNumber() >= user.getPid());
        result.add(new Checker<>(stream, collector));

        collector = userQuery.where(isValid.and(User::getRandomNumber).gt(get(User::getPid)));
        stream = newStream(check).filter(User::isValid)
                .filter(user -> user.getPid() != null && user.getRandomNumber() > user.getPid());
        result.add(new Checker<>(stream, collector));

        collector = userQuery.where(isValid.and(User::getRandomNumber).le(get(User::getPid)));
        stream = newStream(check).filter(User::isValid)
                .filter(user -> user.getPid() != null && user.getRandomNumber() <= user.getPid());
        result.add(new Checker<>(stream, collector));

        collector = userQuery.where(isValid.and(User::getRandomNumber).lt(get(User::getPid)));
        stream = newStream(check).filter(User::isValid)
                .filter(user -> user.getPid() != null && user.getRandomNumber() < user.getPid());
        result.add(new Checker<>(stream, collector));

        collector = userQuery.where(isValid.and(User::getRandomNumber)
                .between(get(User::getRandomNumber), get(User::getPid)));
        stream = newStream(check).filter(User::isValid)
                .filter(user -> user.getPid() != null && user.getRandomNumber() >= user.getRandomNumber() && user.getRandomNumber() <= user.getPid());
        result.add(new Checker<>(stream, collector));
        for (Checker<User, SelectOrderByStep<User, User>> checker : getExpressionOperatorCase(check)) {
            addTestCaseAndCheck(result, checker);
        }
        return result;
    }

    private List<Checker<User, SelectOrderByStep<User, User>>> getExpressionOperatorCase(Checker<User, SelectWhereStep<User, User>> check) {
        List<Checker<User, SelectOrderByStep<User, User>>> result = new ArrayList<>();
        // B eq(U value);
        List<User> users = check.expected.stream().filter(it -> it.getRandomNumber() == 1).collect(Collectors.toList());
        SelectOrderByStep<User, User> collector = check.collector.where(User::getRandomNumber).eq(1);
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(User::getRandomNumber).eq(1);
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).eq(1));
        result.add(new Checker<>(users, collector));
        users = check.expected.stream()
                .filter(it -> it.getRandomNumber() == 1 || it.getRandomNumber() == 2)
                .collect(Collectors.toList());
        collector = check.collector.where(get(User::getRandomNumber).eq(1).or(User::getRandomNumber).eq(2));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).eq(1).or(User::getRandomNumber).eq(2));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).eq(1).or(ImmutableList.of(get(User::getRandomNumber).eq(2))));
        result.add(new Checker<>(users, collector));
        users = check.expected.stream()
                .filter(it -> it.getRandomNumber() == 1 && it.isValid())
                .collect(Collectors.toList());
        collector = check.collector.where(User::getRandomNumber).eq(1).where(User::isValid).eq(true);
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(User::getRandomNumber).eq(1)
                .where((Path<User, Boolean>) User::isValid).eq(Expressions.ofTrue());
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).eq(1).and(User::isValid).eq(true));
        result.add(new Checker<>(users, collector));

        collector = check.collector.where(get(User::getRandomNumber).eq(1).and(ImmutableList.of(get(User::isValid))));
        result.add(new Checker<>(users, collector));

        //
        //    B eq(ExpressionHolder<T, U> expression);
        users = check.expected.stream().filter(it -> it.getRandomNumber() == it.getId()).collect(Collectors.toList());
        collector = check.collector.where(User::getRandomNumber).eq(get(User::getId));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).eq(get(User::getId)));
        result.add(new Checker<>(users, collector));

        //
        //    B ne(U value);
        users = check.expected.stream().filter(it -> it.getRandomNumber() != 1).collect(Collectors.toList());
        collector = check.collector.where(User::getRandomNumber).ne(1);
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).ne(1));
        result.add(new Checker<>(users, collector));
        //
        //    B ne(ExpressionHolder<T, U> expression);
        users = check.expected.stream().filter(it -> it.getRandomNumber() != it.getId()).collect(Collectors.toList());
        collector = check.collector.where(User::getRandomNumber).ne(get(User::getId));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).ne(get(User::getId)));
        result.add(new Checker<>(users, collector));
        users = check.expected.stream().filter(User::isValid).collect(Collectors.toList());
        collector = check.collector.where(User::isValid).eq(true);
        result.add(new Checker<>(users, collector));
        //
        //    @SuppressWarnings({"unchecked"})
        //    B in(U... values);
        for (int i = 0; i <= 5; i++) {
            Integer[] nums = new Integer[i];
            for (int j = 0; j < i; j++) {
                nums[j] = j;
            }
            List<Integer> values = Arrays.asList(nums);
            users = check.expected.stream().filter(it -> values.contains(it.getRandomNumber()))
                    .collect(Collectors.toList());
            collector = check.collector.where(User::getRandomNumber).in(nums);
            result.add(new Checker<>(users, collector));
            collector = check.collector.where(User::getRandomNumber).in(values);
            result.add(new Checker<>(users, collector));
            collector = check.collector.where(get(User::getRandomNumber).in(nums));
            result.add(new Checker<>(users, collector));
            collector = check.collector.where(get(User::getRandomNumber).in(values));
            result.add(new Checker<>(users, collector));

            List<TypedExpression<User, Integer>> collect = values.stream()
                    .<TypedExpression<User, Integer>>map(Expressions::of)
                    .collect(Collectors.toList());
            collector = check.collector.where(User::getRandomNumber).in(collect);
            result.add(new Checker<>(users, collector));
            collector = check.collector.where(get(User::getRandomNumber).in(collect));
            result.add(new Checker<>(users, collector));

            users = check.expected.stream().filter(it -> !values.contains(it.getRandomNumber()))
                    .collect(Collectors.toList());
            collector = check.collector.where(User::getRandomNumber).notIn(nums);
            result.add(new Checker<>(users, collector));
            collector = check.collector.where(get(User::getRandomNumber).notIn(nums));
            result.add(new Checker<>(users, collector));
            collector = check.collector.where(User::getRandomNumber).notIn(values);
            result.add(new Checker<>(users, collector));
            collector = check.collector.where(get(User::getRandomNumber).notIn(values));
            result.add(new Checker<>(users, collector));
            collector = check.collector.where(User::getRandomNumber).notIn(collect);
            result.add(new Checker<>(users, collector));
            collector = check.collector.where(get(User::getRandomNumber).notIn(collect));
            result.add(new Checker<>(users, collector));

        }

        //
        //    B isNull();
        users = check.expected.stream().filter(it -> it.getPid() == null).collect(Collectors.toList());
        collector = check.collector.where(User::getPid).isNull();
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getPid).isNull());
        result.add(new Checker<>(users, collector));
        //
        //    B isNotNull();
        users = check.expected.stream().filter(it -> it.getPid() != null).collect(Collectors.toList());
        collector = check.collector.where(User::getPid).isNotNull();
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getPid).isNotNull());
        result.add(new Checker<>(users, collector));


        //  B ge(U value);
        users = check.expected.stream().filter(it -> it.getRandomNumber() >= 50).collect(Collectors.toList());
        collector = check.collector.where(User::getRandomNumber).ge(50);
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).ge(50));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(User::getRandomNumber).ge(Expressions.of(50));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).ge(Expressions.of(50)));
        result.add(new Checker<>(users, collector));

        //
        //        B gt(U value);
        users = check.expected.stream().filter(it -> it.getRandomNumber() > 50).collect(Collectors.toList());
        collector = check.collector.where(User::getRandomNumber).gt(50);
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).gt(50));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(User::getRandomNumber).gt(Expressions.of(50));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).gt(Expressions.of(50)));
        result.add(new Checker<>(users, collector));
        //
        //        B le(U value);
        users = check.expected.stream().filter(it -> it.getRandomNumber() <= 50).collect(Collectors.toList());
        collector = check.collector.where(User::getRandomNumber).le(50);
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).le(50));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(User::getRandomNumber).le(Expressions.of(50));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).le(Expressions.of(50)));
        result.add(new Checker<>(users, collector));
        //
        //        B lt(U value);
        users = check.expected.stream().filter(it -> it.getRandomNumber() < 50).collect(Collectors.toList());
        collector = check.collector.where(User::getRandomNumber).lt(50);
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).lt(50));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(User::getRandomNumber).lt(Expressions.of(50));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).lt(Expressions.of(50)));
        result.add(new Checker<>(users, collector));
        //
        //        B between(U l, U r);

        users = check.expected.stream().filter(it -> {
                    int number = it.getRandomNumber();
                    return number >= 25 && number <= 73;
                })
                .collect(Collectors.toList());
        collector = check.collector.where(User::getRandomNumber).between(25, 73);
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).between(25, 73));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(User::getRandomNumber).between(25, Expressions.of(73));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).between(25, Expressions.of(73)));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(User::getRandomNumber).between(Expressions.of(25), 73);
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).between(Expressions.of(25), 73));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(User::getRandomNumber)
                .between(Expressions.of(25), Expressions.of(73));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber)
                .between(Expressions.of(25), Expressions.of(73)));
        result.add(new Checker<>(users, collector));

        //
        //        B notBetween(U l, U r);
        users = check.expected.stream().filter(it -> {
                    int number = it.getRandomNumber();
                    return number < 25 || number > 73;
                })
                .collect(Collectors.toList());
        collector = check.collector.where(User::getRandomNumber).notBetween(25, 73);
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).notBetween(25, 73));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(User::getRandomNumber).notBetween(25, Expressions.of(73));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).notBetween(25, Expressions.of(73)));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(User::getRandomNumber).notBetween(Expressions.of(25), 73);
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).notBetween(Expressions.of(25), 73));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(User::getRandomNumber)
                .notBetween(Expressions.of(25), Expressions.of(73));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber)
                .notBetween(Expressions.of(25), Expressions.of(73)));
        result.add(new Checker<>(users, collector));

        //   NumberOperator<T, U, B> add(U value);
        users = check.expected.stream().filter(it -> it.getRandomNumber() + 1 == 5).collect(Collectors.toList());
        collector = check.collector.where(User::getRandomNumber).add(1).eq(5);
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).add(1).eq(5));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(User::getRandomNumber).add(Expressions.of(1)).eq(5);
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).add(Expressions.of(1)).eq(5));
        result.add(new Checker<>(users, collector));

        //
        //        NumberOperator<T, U, B> subtract(U value);
        users = check.expected.stream().filter(it -> it.getRandomNumber() - 1 == 5).collect(Collectors.toList());
        collector = check.collector.where(User::getRandomNumber).subtract(1).eq(5);
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).subtract(1).eq(5));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(User::getRandomNumber).subtract(Expressions.of(1)).eq(5);
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).subtract(Expressions.of(1)).eq(5));
        result.add(new Checker<>(users, collector));
        //
        //        NumberOperator<T, U, B> multiply(U value);
        users = check.expected.stream().filter(it -> it.getRandomNumber() * 3 == 45).collect(Collectors.toList());
        collector = check.collector.where(User::getRandomNumber).multiply(3).eq(45);
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).multiply(3).eq(45));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(User::getRandomNumber).multiply(Expressions.of(3)).eq(45);
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).multiply(Expressions.of(3)).eq(45));
        result.add(new Checker<>(users, collector));
        //
        //        NumberOperator<T, U, B> divide(U value);
        users = check.expected.stream().filter(it -> it.getRandomNumber() / 3. == 12).collect(Collectors.toList());
        collector = check.collector.<Number>where(User::getRandomNumber).divide(3.0).eq(12);
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(Paths.<User, Number>get(User::getRandomNumber).divide(3.).eq(12));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(User::getRandomNumber).divide(Expressions.of(3)).eq(12);
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).divide(Expressions.of(3)).eq(12));
        result.add(new Checker<>(users, collector));

        //
        //        NumberOperator<T, U, B> mod(U value);

        users = check.expected.stream().filter(it -> it.getRandomNumber() % 8 == 2).collect(Collectors.toList());
        collector = check.collector.where(User::getRandomNumber).mod(8).eq(2);
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).mod(8).eq(2));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(User::getRandomNumber).mod(Expressions.of(8)).eq(2);
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).mod(Expressions.of(8)).eq(2));
        result.add(new Checker<>(users, collector));


        //   B like(String value);

        users = check.expected.stream().filter(it -> it.getUsername().contains("one")).collect(Collectors.toList());
        collector = check.collector.where(User::getUsername).like("%one%");
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getUsername).like("%one%"));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(User::getUsername).contains("one");
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getUsername).contains("one"));
        result.add(new Checker<>(users, collector));
        //
        //        default B startWith(String value) {
        //            return like(value + '%');
        //        }
        users = check.expected.stream().filter(it -> it.getUsername().startsWith("Ja")).collect(Collectors.toList());
        collector = check.collector.where(User::getUsername).startWith("Ja");
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getUsername).startWith("Ja"));
        result.add(new Checker<>(users, collector));
        //
        //        default B endsWith(String value) {
        //            return like('%' + value);
        //        }
        users = check.expected.stream().filter(it -> it.getUsername().endsWith("win")).collect(Collectors.toList());
        collector = check.collector.where(User::getUsername).endsWith("win");
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getUsername).endsWith("win"));
        result.add(new Checker<>(users, collector));


        ////
        users = check.expected.stream().filter(it -> !it.getUsername().contains("one")).collect(Collectors.toList());
        collector = check.collector.where(User::getUsername).notLike("%one%");
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getUsername).notLike("%one%"));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(User::getUsername).notContains("one");
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getUsername).notContains("one"));
        result.add(new Checker<>(users, collector));
        //
        //        default B startWith(String value) {
        //            return like(value + '%');
        //        }
        users = check.expected.stream().filter(it -> !it.getUsername().startsWith("Ja")).collect(Collectors.toList());
        collector = check.collector.where(User::getUsername).notStartWith("Ja");
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getUsername).notStartWith("Ja"));
        result.add(new Checker<>(users, collector));
        //
        //        default B endsWith(String value) {
        //            return like('%' + value);
        //        }
        users = check.expected.stream().filter(it -> !it.getUsername().endsWith("win")).collect(Collectors.toList());
        collector = check.collector.where(User::getUsername).notEndsWith("win");
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getUsername).notEndsWith("win"));
        result.add(new Checker<>(users, collector));

        //
        //        StringOperator<T, B> lower();
        users = check.expected.stream().filter(it -> !it.getUsername().toLowerCase().startsWith("ja")).collect(Collectors.toList());
        collector = check.collector.where(User::getUsername).lower().notStartWith("ja");
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getUsername).lower().notStartWith("ja"));
        result.add(new Checker<>(users, collector));
        //
        //        StringOperator<T, B> upper();
        users = check.expected.stream().filter(it -> !it.getUsername().toUpperCase().startsWith("JA")).collect(Collectors.toList());
        collector = check.collector.where(User::getUsername).upper().notStartWith("JA");
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getUsername).upper().notStartWith("JA"));
        result.add(new Checker<>(users, collector));
        //
        //        StringOperator<T, B> substring(int a, int b);
        users = check.expected.stream().filter(it -> it.getUsername().startsWith("ar", 1)).collect(Collectors.toList());
        collector = check.collector.where(User::getUsername).substring(2, 2).eq("ar");
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getUsername).substring(2, 2).eq("ar"));
        result.add(new Checker<>(users, collector));
        //
        //        StringOperator<T, B> substring(int a);
        users = check.expected.stream().filter(it -> {
                    String username = it.getUsername();
                    return username.length() == 17 && username.endsWith("ing");
                })
                .collect(Collectors.toList());
        collector = check.collector.where(User::getUsername).substring(15).eq("ing");
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getUsername).substring(15).eq("ing"));
        result.add(new Checker<>(users, collector));

        users = check.expected.stream().filter(it -> {
                    String username = it.getUsername();
                    return username.length() == 17;
                })
                .collect(Collectors.toList());
        collector = check.collector.where(User::getUsername).length().eq(17);
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getUsername).length().eq(17));
        result.add(new Checker<>(users, collector));

        users = check.expected.stream().filter(it -> {
                    String username = it.getUsername();
                    return "Trim".equals(username.trim());
                })
                .collect(Collectors.toList());
        collector = check.collector.where(User::getUsername).trim().eq("Trim");
        result.add(new Checker<>(users, collector));

        return result;
    }

    private static <T, U extends Collector<T>> void addTestCaseAndCheck(List<Checker<T, U>> result, Checker<T, U> checker) {
        result.add(checker);
    }

    private static <T> Stream<T> newStream(Checker<T, ?> check) {
        return check.expected.stream();
    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    void queryList(UserRepository userQuery) {
        User single = userQuery
                .where(User::getId).le(10)
                .getSingle(10);
        User user = userQuery.users().get(10);
        assertEquals(single, user);
        single = userQuery
                .where(User::getId).eq(10)
                .single().orElse(null);
        User user10 = null;
        for (User u : userQuery.users()) {
            if (u.getId() == 10) {
                user10 = u;
                break;
            }
        }
        assertEquals(user10, single);

        single = userQuery
                .where(User::getId).le(10)
                .single(10).orElse(null);
        assertEquals(single, user);

        assertFalse(userQuery
                .where(User::getId).le(10)
                .single(11).isPresent());

        Slice<User> slice = userQuery.slice(20, 10);
        assertEquals(slice.total(), userQuery.users().size());
        List<User> list = userQuery.users().stream()
                .skip(20)
                .limit(10)
                .collect(Collectors.toList());
        assertEquals(slice.data(), list);

        Page<User> page = userQuery.slice(new Pageable<>(3, 10));
        assertEquals(page.getTotal(), userQuery.users().size());
        assertEquals(page.getList(), list);

        page = userQuery
                .orderBy(User::getId)
                .slice(new Pageable<>(3, 10));
        assertEquals(page.getTotal(), userQuery.users().size());
        assertEquals(page.getList(), list);

        user = userQuery.getFirst();
        assertEquals(user, userQuery.users().get(0));
        user = userQuery.getFirst(10);
        assertEquals(user, userQuery.users().get(10));
        user = userQuery.first().orElse(null);
        assertEquals(user, userQuery.users().get(0));
        user = userQuery.first(8).orElse(null);
        assertEquals(user, userQuery.users().get(8));

    }


    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    void lock(UserRepository userQuery) {
        userQuery.doInTransaction(() -> testLock(userQuery));
    }

    private static void testLock(UserRepository userQuery) {
        for (LockModeType lockModeType : LockModeType.values()) {
            try {
                User single = userQuery
                        .where(User::getId).le(10)
                        .getSingle(10, lockModeType);
                User user = userQuery.users().get(10);
                assertEquals(single, user);
                single = userQuery
                        .where(User::getId).eq(10)
                        .single(lockModeType).orElse(null);
                User user10 = null;
                for (User u : userQuery.users()) {
                    if (u.getId() == 10) {
                        user10 = u;
                        break;
                    }
                }
                assertEquals(user10, single);

                single = userQuery
                        .where(User::getId).le(10)
                        .single(10).orElse(null);
                assertEquals(single, user);

                assertFalse(userQuery
                        .where(User::getId).le(10)
                        .single(11, lockModeType).isPresent());


                user = userQuery.getFirst(lockModeType);
                assertEquals(user, userQuery.users().get(0));
                user = userQuery.getFirst(10, lockModeType);
                assertEquals(user, userQuery.users().get(10));
                user = userQuery.first(lockModeType).orElse(null);
                assertEquals(user, userQuery.users().get(0));
                user = userQuery.first(8, lockModeType).orElse(null);
                assertEquals(user, userQuery.users().get(8));

                user = userQuery.where(User::getId).eq(0)
                        .requireSingle(lockModeType);
                assertEquals(user, userQuery.users().get(0));

                List<User> users = userQuery.where(User::getId).eq(0)
                        .getList(lockModeType);
                assertEquals(users.get(0), userQuery.users().get(0));
                users = userQuery.where(User::getId).eq(0)
                        .getList(lockModeType);
                assertEquals(users.get(0), userQuery.users().get(0));

            } catch (Exception e) {
                log.error(lockModeType.name(), e);
            }
        }
    }

    static class Checker<T, U extends Collector<T>> {
        List<T> expected;

        U collector;

        Checker(Stream<T> expected, U collector) {
            this(expected.collect(Collectors.toList()), collector);
        }

        Checker(List<T> expected, U collector) {
            this.expected = expected;
            this.collector = collector;
            check();
        }

        void check() {
            List<T> actual = collector.getList();
            assertEquals(expected, actual);
        }

    }
}