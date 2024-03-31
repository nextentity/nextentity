package io.github.nextentity.test;

import io.github.nextentity.core.Tuples;
import io.github.nextentity.core.TypedExpressions;
import io.github.nextentity.core.api.Lists;
import io.github.nextentity.core.api.LockModeType;
import io.github.nextentity.core.api.Path;
import io.github.nextentity.core.api.Path.ComparablePath;
import io.github.nextentity.core.api.Query;
import io.github.nextentity.core.api.Query.ExpressionsBuilder;
import io.github.nextentity.core.api.Query.OrderBy;
import io.github.nextentity.core.api.Query.Select;
import io.github.nextentity.core.api.Query.Where;
import io.github.nextentity.core.api.Root;
import io.github.nextentity.core.api.Slice;
import io.github.nextentity.core.api.TypedExpression;
import io.github.nextentity.core.api.TypedExpression.BooleanExpression;
import io.github.nextentity.core.util.Paths;
import io.github.nextentity.core.util.tuple.Tuple;
import io.github.nextentity.core.util.tuple.Tuple2;
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
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static io.github.nextentity.core.util.Paths.get;
import static io.github.nextentity.test.Transaction.doInTransaction;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

@Slf4j
class QueryBuilderTest {

    static List<User> users() {
        return UserQueryProvider.users();
    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    void select(Select<User> userQuery) {

        int offset = 90;
        User f2 = userQuery.fetch(User::getParentUser).getFirst(offset);
        IUser first1 = userQuery.select(IUser.class).getFirst(offset);
        Assertions.assertEquals(first1.getUsername(), f2.getUsername());
        Assertions.assertEquals(first1.getId(), f2.getId());
        Assertions.assertEquals(first1.getRandomNumber(), f2.getRandomNumber());
        if (f2.getParentUser() != null) {
            Assertions.assertEquals(first1.getParentUser().username(), f2.getParentUser().getUsername());
            Assertions.assertEquals(first1.getParentUser().id(), f2.getParentUser().getId());
            Assertions.assertEquals(first1.getParentUser().randomNumber(), f2.getParentUser().getRandomNumber());
        }

        User first3 = userQuery.fetch(User::getParentUser).getFirst(offset);
        System.out.println(first3);
        System.out.println(first3.getParentUser());
        IUser.U first2 = userQuery.select(IUser.U.class).getFirst(offset);
        Assertions.assertEquals(first2.username(), f2.getUsername());
        Assertions.assertEquals(first2.id(), f2.getId());
        Assertions.assertEquals(first2.randomNumber(), f2.getRandomNumber());

        User first = userQuery.select(User.class).getFirst();
        assertEquals(first, users().get(0));

        Integer firstUserid = userQuery.select(User::getId).getFirst();
        assertEquals(firstUserid, users().get(0).getId());

        Tuple2<Integer, Integer> array = userQuery.select(User::getId, User::getRandomNumber).getFirst();
        assertEquals(array.get0(), users().get(0).getId());
        assertEquals(array.get1(), users().get(0).getRandomNumber());

        UserModel model = userQuery.select(UserModel.class).getFirst();
        assertEquals(model, new UserModel(users().get(0)));

        UserInterface ui = userQuery.select(UserInterface.class).getFirst();
        assertEquals(model.asMap(), ui.asMap());

        ui = userQuery.selectDistinct(UserInterface.class).getFirst();
        assertEquals(model.asMap(), ui.asMap());

        Long count = userQuery.select(get(User::getId).count()).getSingle();
        assertEquals(count, users().size());

        Tuple aggArray = userQuery.select(Lists.of(
                get(User::getId).count(),
                get(User::getRandomNumber).max(),
                get(User::getRandomNumber).min(),
                get(User::getRandomNumber).sum(),
                get(User::getRandomNumber).avg()
        )).getSingle();

        int max = Integer.MIN_VALUE;
        int min = Integer.MAX_VALUE;
        int sum = 0;
        for (User user : users()) {
            int number = user.getRandomNumber();
            max = Math.max(max, number);
            min = Math.min(min, number);
            sum += number;
        }

        assertEquals(aggArray.<Long>get(0), users().size());
        assertEquals(aggArray.<Integer>get(1), max);
        assertEquals(aggArray.<Integer>get(2), min);
        assertEquals(aggArray.<Number>get(3).intValue(), sum);
        assertEquals(aggArray.<Number>get(4).doubleValue(), sum * 1.0 / users().size(), 0.001);


        assertEquals(
                userQuery
                        .select(User::getId, User::getRandomNumber)
                        .getList(),

                users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber()))
                        .collect(Collectors.toList())
        );

        assertEquals(
                userQuery
                        .select(User::getId, User::getRandomNumber, User::getTime)
                        .getList(),

                users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber(), it.getTime()))
                        .collect(Collectors.toList())
        );

        assertEquals(
                userQuery
                        .select(User::getId, User::getRandomNumber, User::getTime, User::getPid)
                        .getList(),

                users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber(), it.getTime(), it.getPid()))
                        .collect(Collectors.toList())
        );


        assertEquals(
                userQuery
                        .select(User::getId, User::getRandomNumber, User::getTime, User::getPid,
                                User::getTimestamp)
                        .getList(),

                users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber(), it.getTime(), it.getPid(),
                                it.getTimestamp()))
                        .collect(Collectors.toList())
        );

        assertEquals(
                userQuery
                        .select(User::getId, User::getRandomNumber, User::getTime, User::getPid,
                                User::getTimestamp, User::isValid)
                        .getList(),

                users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber(), it.getTime(), it.getPid(),
                                it.getTimestamp(), it.isValid()))
                        .collect(Collectors.toList())
        );

        assertEquals(
                userQuery
                        .select(User::getId, User::getRandomNumber, User::getTime, User::getPid,
                                User::getTimestamp, User::isValid, User::getGender)
                        .getList(),

                users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber(), it.getTime(), it.getPid(),
                                it.getTimestamp(), it.isValid(), it.getGender()))
                        .collect(Collectors.toList())
        );

        assertEquals(
                userQuery
                        .select(User::getId, User::getRandomNumber, User::getTime, User::getPid,
                                User::getTimestamp, User::isValid, User::getGender, User::getInstant)
                        .getList(),

                users().stream()
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

                users().stream()
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

                users().stream()
                        .map(user -> Tuples.of(
                                user.getId(), user.getRandomNumber(), user.getTime(), user.getPid(),
                                user.getTimestamp(), user.isValid(), user.getGender(), user.getInstant(),
                                user.getTestLong(), user.getTestInteger()
                        ))
                        .collect(Collectors.toList())
        );


        assertEquals(
                userQuery
                        .select(Lists.<Path<User, ?>>of(User::getId, User::getRandomNumber, User::getTime, User::getPid,
                                User::getTimestamp, User::isValid, User::getGender, User::getInstant,
                                User::getTestLong, User::getTestInteger))
                        .getList(),

                users().stream()
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

                users().stream()
                        .map(user -> Tuples.of(
                                user.getId(), user.getRandomNumber(), user.getTime(), user.getPid(),
                                user.getTimestamp(), user.isValid(), user.getGender(), user.getInstant(),
                                user.getTestLong(), user.getTestInteger()
                        ))
                        .collect(Collectors.toList())
        );


        assertEquals(
                userQuery
                        .selectDistinct(User::getTestLocalDate, User::getRandomNumber)
                        .getList(),

                users().stream()
                        .map(it -> Tuples.of(it.getTestLocalDate(), it.getRandomNumber()))
                        .distinct()
                        .collect(Collectors.toList())
        );

        assertEquals(
                userQuery
                        .selectDistinct(User::getTestLocalDate, User::getRandomNumber, User::getTime)
                        .getList(),

                users().stream()
                        .map(it -> Tuples.of(it.getTestLocalDate(), it.getRandomNumber(), it.getTime()))
                        .distinct()
                        .collect(Collectors.toList())
        );

        assertEquals(
                userQuery
                        .selectDistinct(User::getTestLocalDate, User::getRandomNumber, User::getTime, User::getPid)
                        .getList(),

                users().stream()
                        .map(it -> Tuples.of(it.getTestLocalDate(), it.getRandomNumber(), it.getTime(), it.getPid()))
                        .distinct()
                        .collect(Collectors.toList())
        );


        assertEquals(
                userQuery
                        .selectDistinct(User::getTestLocalDate, User::getRandomNumber, User::getTime, User::getPid,
                                User::getTimestamp)
                        .getList(),

                users().stream()
                        .map(it -> Tuples.of(it.getTestLocalDate(), it.getRandomNumber(), it.getTime(), it.getPid(),
                                it.getTimestamp()))
                        .distinct()
                        .collect(Collectors.toList())
        );

        assertEquals(
                userQuery
                        .selectDistinct(User::getTestLocalDate, User::getRandomNumber, User::getTime, User::getPid,
                                User::getTimestamp, User::isValid)
                        .getList(),

                users().stream()
                        .map(it -> Tuples.of(it.getTestLocalDate(), it.getRandomNumber(), it.getTime(), it.getPid(),
                                it.getTimestamp(), it.isValid()))
                        .distinct()
                        .collect(Collectors.toList())
        );

        assertEquals(
                userQuery
                        .selectDistinct(User::getTestLocalDate, User::getRandomNumber, User::getTime, User::getPid,
                                User::getTimestamp, User::isValid, User::getGender)
                        .getList(),

                users().stream()
                        .map(it -> Tuples.of(it.getTestLocalDate(), it.getRandomNumber(), it.getTime(), it.getPid(),
                                it.getTimestamp(), it.isValid(), it.getGender()))
                        .distinct()
                        .collect(Collectors.toList())
        );

        assertEquals(
                userQuery
                        .selectDistinct(User::getTestLocalDate, User::getRandomNumber, User::getTime, User::getPid,
                                User::getTimestamp, User::isValid, User::getGender, User::getInstant)
                        .getList(),

                users().stream()
                        .map(it -> Tuples.of(it.getTestLocalDate(), it.getRandomNumber(), it.getTime(), it.getPid(),
                                it.getTimestamp(), it.isValid(), it.getGender(), it.getInstant()))
                        .distinct()
                        .collect(Collectors.toList())
        );

        assertEquals(
                userQuery
                        .selectDistinct(User::getTestLocalDate, User::getRandomNumber, User::getTime, User::getPid,
                                User::getTimestamp, User::isValid, User::getGender, User::getInstant,
                                User::getTestLong)
                        .getList(),

                users().stream()
                        .map(it -> Tuples.of(it.getTestLocalDate(), it.getRandomNumber(), it.getTime(), it.getPid(),
                                it.getTimestamp(), it.isValid(), it.getGender(), it.getInstant(),
                                it.getTestLong()))
                        .distinct()
                        .collect(Collectors.toList())
        );

        assertEquals(
                userQuery
                        .selectDistinct(User::getTestLocalDate, User::getRandomNumber, User::getTime, User::getPid,
                                User::getTimestamp, User::isValid, User::getGender, User::getInstant,
                                User::getTestLong, User::getTestInteger)
                        .getList(),

                users().stream()
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
                        .selectDistinct(User::getTestLocalDate, User::getRandomNumber, User::getTime, User::getPid,
                                User::getTimestamp, User::isValid, User::getGender, User::getInstant,
                                User::getTestLong, User::getTestInteger)
                        .getList(),

                users().stream()
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

                users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber()))
                        .distinct()
                        .collect(Collectors.toList())
        );

        assertEquals(
                userQuery
                        .select(User::getId, User::getRandomNumber, User::getTime)
                        .groupBy(User::getId, User::getRandomNumber, User::getTime)
                        .getList(),

                users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber(), it.getTime()))
                        .distinct()
                        .collect(Collectors.toList())
        );

        assertEquals(
                userQuery
                        .select(User::getId, User::getRandomNumber, User::getTime, User::getPid)
                        .groupBy(User::getId, User::getRandomNumber, User::getTime, User::getPid)
                        .getList(),

                users().stream()
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

                users().stream()
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

                users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber(), it.getTime(), it.getPid(),
                                it.getTimestamp(), it.isValid()))
                        .distinct()
                        .collect(Collectors.toList())
        );


        assertEquals(
                userQuery
                        .select(get(User::getId), get(User::getRandomNumber))
                        .getList(),

                users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber()))
                        .collect(Collectors.toList())
        );

        assertEquals(
                userQuery
                        .select(get(User::getId), get(User::getRandomNumber), get(User::getTime))
                        .getList(),

                users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber(), it.getTime()))
                        .collect(Collectors.toList())
        );

        assertEquals(
                userQuery
                        .select(get(User::getId), get(User::getRandomNumber), get(User::getTime), get(User::getPid))
                        .getList(),

                users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber(), it.getTime(), it.getPid()))
                        .collect(Collectors.toList())
        );


        assertEquals(
                userQuery
                        .select(get(User::getId), get(User::getRandomNumber), get(User::getTime), get(User::getPid),
                                get(User::getTimestamp))
                        .getList(),

                users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber(), it.getTime(), it.getPid(),
                                it.getTimestamp()))
                        .collect(Collectors.toList())
        );

        assertEquals(
                userQuery
                        .select(get(User::getId), get(User::getRandomNumber), get(User::getTime), get(User::getPid),
                                get(User::getTimestamp), get(User::isValid))
                        .getList(),

                users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber(), it.getTime(), it.getPid(),
                                it.getTimestamp(), it.isValid()))
                        .collect(Collectors.toList())
        );

        assertEquals(
                userQuery
                        .select(get(User::getId), get(User::getRandomNumber), get(User::getTime), get(User::getPid),
                                get(User::getTimestamp), get(User::isValid), get(User::getGender))
                        .getList(),

                users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber(), it.getTime(), it.getPid(),
                                it.getTimestamp(), it.isValid(), it.getGender()))
                        .collect(Collectors.toList())
        );

        assertEquals(
                userQuery
                        .select(get(User::getId), get(User::getRandomNumber), get(User::getTime), get(User::getPid),
                                get(User::getTimestamp), get(User::isValid), get(User::getGender), get(User::getInstant))
                        .getList(),

                users().stream()
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

                users().stream()
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

                users().stream()
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

                users().stream()
                        .map(user -> Tuples.of(
                                user.getId(), user.getRandomNumber(), user.getTime(), user.getPid(),
                                user.getTimestamp(), user.isValid(), user.getGender(), user.getInstant(),
                                user.getTestLong(), user.getTestInteger()
                        ))
                        .collect(Collectors.toList())
        );


        assertEquals(
                userQuery
                        .selectDistinct(get(User::getId), get(User::getRandomNumber))
                        .getList(),

                users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber()))
                        .distinct()
                        .collect(Collectors.toList())
        );

        assertEquals(
                userQuery
                        .selectDistinct(get(User::getId), get(User::getRandomNumber), get(User::getTime))
                        .getList(),

                users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber(), it.getTime()))
                        .collect(Collectors.toList())
        );

        assertEquals(
                userQuery
                        .selectDistinct(get(User::getId), get(User::getRandomNumber), get(User::getTime), get(User::getPid))
                        .getList(),

                users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber(), it.getTime(), it.getPid()))
                        .collect(Collectors.toList())
        );


        assertEquals(
                userQuery
                        .selectDistinct(get(User::getId), get(User::getRandomNumber), get(User::getTime), get(User::getPid),
                                get(User::getTimestamp))
                        .getList(),

                users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber(), it.getTime(), it.getPid(),
                                it.getTimestamp()))
                        .collect(Collectors.toList())
        );

        assertEquals(
                userQuery
                        .selectDistinct(get(User::getId), get(User::getRandomNumber), get(User::getTime), get(User::getPid),
                                get(User::getTimestamp), get(User::isValid))
                        .getList(),

                users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber(), it.getTime(), it.getPid(),
                                it.getTimestamp(), it.isValid()))
                        .collect(Collectors.toList())
        );

        assertEquals(
                userQuery
                        .selectDistinct(get(User::getId), get(User::getRandomNumber), get(User::getTime), get(User::getPid),
                                get(User::getTimestamp), get(User::isValid), get(User::getGender))
                        .getList(),

                users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber(), it.getTime(), it.getPid(),
                                it.getTimestamp(), it.isValid(), it.getGender()))
                        .collect(Collectors.toList())
        );

        assertEquals(
                userQuery
                        .selectDistinct(get(User::getId), get(User::getRandomNumber), get(User::getTime), get(User::getPid),
                                get(User::getTimestamp), get(User::isValid), get(User::getGender), get(User::getInstant))
                        .getList(),

                users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber(), it.getTime(), it.getPid(),
                                it.getTimestamp(), it.isValid(), it.getGender(), it.getInstant()))
                        .collect(Collectors.toList())
        );

        assertEquals(
                userQuery
                        .selectDistinct(get(User::getId), get(User::getRandomNumber), get(User::getTime), get(User::getPid),
                                get(User::getTimestamp), get(User::isValid), get(User::getGender), get(User::getInstant),
                                get(User::getTestLong))
                        .getList(),

                users().stream()
                        .map(it -> Tuples.of(it.getId(), it.getRandomNumber(), it.getTime(), it.getPid(),
                                it.getTimestamp(), it.isValid(), it.getGender(), it.getInstant(),
                                it.getTestLong()))
                        .collect(Collectors.toList())
        );

        assertEquals(
                userQuery
                        .selectDistinct(get(User::getId), get(User::getRandomNumber), get(User::getTime), get(User::getPid),
                                get(User::getTimestamp), get(User::isValid), get(User::getGender), get(User::getInstant),
                                get(User::getTestLong), get(User::getTestInteger))
                        .getList(),

                users().stream()
                        .map(user -> Tuples.of(
                                user.getId(), user.getRandomNumber(), user.getTime(), user.getPid(),
                                user.getTimestamp(), user.isValid(), user.getGender(), user.getInstant(),
                                user.getTestLong(), user.getTestInteger()
                        ))
                        .collect(Collectors.toList())
        );

        assertEquals(
                userQuery
                        .selectDistinct(get(User::getId), get(User::getRandomNumber), get(User::getTime), get(User::getPid),
                                get(User::getTimestamp), get(User::isValid), get(User::getGender), get(User::getInstant),
                                get(User::getTestLong), get(User::getTestInteger))
                        .getList(),

                users().stream()
                        .map(user -> Tuples.of(
                                user.getId(), user.getRandomNumber(), user.getTime(), user.getPid(),
                                user.getTimestamp(), user.isValid(), user.getGender(), user.getInstant(),
                                user.getTestLong(), user.getTestInteger()
                        ))
                        .collect(Collectors.toList())
        );


    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    void selectDistinct(Select<User> userQuery) {
        List<Integer> list = userQuery.selectDistinct(User::getRandomNumber)
                .getList();
        List<Integer> collect = users()
                .stream().map(User::getRandomNumber)
                .distinct()
                .collect(Collectors.toList());
        assertEquals(list, collect);


        list = userQuery.selectDistinct(get(User::getRandomNumber))
                .getList();
        assertEquals(list, collect);

        List<Tuple> tuples = userQuery.selectDistinct(Lists.<Path<User, ?>>of(User::getRandomNumber, User::getUsername))
                .getList();

        List<Tuple> collect2 = users().stream()
                .map(user -> Tuples.of(user.getRandomNumber(), user.getUsername()))
                .distinct()
                .collect(Collectors.toList());

        assertEquals(tuples, collect2);

        tuples = userQuery.selectDistinct((ExpressionsBuilder<User>) user ->
                        Lists.of(user.get(User::getRandomNumber), user.get(User::getUsername)))
                .getList();

        assertEquals(tuples, collect2);


        tuples = userQuery.select((ExpressionsBuilder<User>) user ->
                        Lists.of(user.get(User::getRandomNumber), user.get(User::getUsername)))
                .getList();
        collect2 = users().stream()
                .map(user -> Tuples.of(user.getRandomNumber(), user.getUsername()))
                .collect(Collectors.toList());
        assertEquals(tuples, collect2);

    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    void fetch(Select<User> userQuery) {
        List<User> users = userQuery.fetch(User::getParentUser).getList();

        assertEquals(users, users());
        for (int i = 0; i < users().size(); i++) {
            User a = users.get(i);
            User b = users().get(i);
            if (b.getParentUser() != null) {
                assertEquals(b.getParentUser(), a.getParentUser());
            } else {
                assertNull(a.getParentUser());
            }
        }

        users = userQuery.fetch(Paths.get(User::getParentUser).get(User::getParentUser))
                .getList();

        assertEquals(users, users());
        for (int i = 0; i < users().size(); i++) {
            User a = users.get(i);
            User b = users().get(i);
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
    void groupBy(Select<User> userQuery) {
        List<Tuple> list = userQuery
                .select(Lists.of(
                        get(User::getRandomNumber),
                        get(User::getId).count()
                ))
                .groupBy(User::getRandomNumber)
                .getList();

        Map<Object, Long> count = users().stream()
                .collect(Collectors.groupingBy(User::getRandomNumber, Collectors.counting()));

        assertEquals(list.size(), count.size());
        for (Tuple objects : list) {
            Long value = count.get(objects.get(0));
            assertEquals(value, objects.get(1));
        }

        ExpressionsBuilder<User> expressionsBuilder =
                (Root<User> root) -> Lists.of(root.get(User::getRandomNumber));
        list = userQuery
                .select(Lists.of(
                        get(User::getRandomNumber),
                        get(User::getId).count()
                ))
                .groupBy(expressionsBuilder)
                .getList();

        assertEquals(list.size(), count.size());
        for (Tuple objects : list) {
            Long value = count.get(objects.get(0));
            assertEquals(value, objects.get(1));
        }

        list = userQuery
                .select(Lists.of(
                        get(User::getRandomNumber),
                        get(User::getId).count()
                ))
                .groupBy(Lists.<Path<User, ?>>of(User::getRandomNumber))
                .getList();

        assertEquals(list.size(), count.size());
        for (Tuple objects : list) {
            Long value = count.get(objects.get(0));
            assertEquals(value, objects.get(1));
        }

        list = userQuery
                .select(Lists.of(
                        get(User::getRandomNumber),
                        get(User::getId).count()
                ))
                .where(User::isValid).eq(true)
                .where(User::getRandomNumber).eq(1)
                .groupBy(expressionsBuilder)
                .getList();
        count = users().stream()
                .filter(it -> it.isValid() && it.getRandomNumber() == 1)
                .collect(Collectors.groupingBy(User::getRandomNumber, Collectors.counting()));
        assertEquals(list.size(), count.size());
        for (Tuple objects : list) {
            Long value = count.get(objects.get(0));
            assertEquals(value, objects.get(1));
        }

    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    void orderBy(Select<User> userQuery) {
        testOrderBy(Lists.of(new Checker<>(users(), userQuery)));
    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    void combinatorial(Select<User> userQuery) {
        testOrderBy(getWhereTestCase(new Checker<>(users(), userQuery)));

    }

    private static void testOrderBy(List<Checker<User, OrderBy<User, User>>> testcase) {
        for (Checker<User, OrderBy<User, User>> checker : testcase) {
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
            Root<User> root = checker.collector
                    .orderBy(User::getRandomNumber, User::getId).root();
            assertEquals(root, Paths.root());

            users = checker.collector
                    .orderBy((Root<User> r) -> Lists.of(
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
    void getList(Select<User> userQuery) {

        List<User> users = userQuery.getList();
        assertEquals(users.size(), users().size());

        users = userQuery.getList(0, 10);
        assertEquals(users, users().subList(0, 10));

        users = userQuery.getList(100, 15);
        assertEquals(users, users().subList(100, 115));

    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    void where(Select<User> userQuery) {
        Checker<User, Where<User, User>> check = new Checker<>(users(), userQuery);
        getWhereTestCase(check);
    }

    private List<Checker<User, OrderBy<User, User>>> whereTestCase;


    private List<Checker<User, OrderBy<User, User>>> getWhereTestCase(Checker<User, Where<User, User>> check) {
        if (whereTestCase != null) {
            return whereTestCase;
        }
        List<Checker<User, OrderBy<User, User>>> result = whereTestCase = new ArrayList<>();
        String username = users().get(10).getUsername();

        Where<User, User> userQuery = check.collector;
        OrderBy<User, User> collector = userQuery
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


        BooleanExpression<User> isValid = get(User::isValid);
        collector = userQuery.where(isValid);
        stream = users().stream().filter(User::isValid);

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
        for (Checker<User, OrderBy<User, User>> checker : getExpressionOperatorCase(check)) {
            addTestCaseAndCheck(result, checker);
        }
        return result;
    }

    private List<Checker<User, OrderBy<User, User>>> getExpressionOperatorCase(Checker<User, Where<User, User>> check) {
        List<Checker<User, OrderBy<User, User>>> result = new ArrayList<>();
        // B eq(U value);
        List<User> users = check.expected.stream().filter(it -> it.getRandomNumber() == 1).collect(Collectors.toList());
        OrderBy<User, User> collector = check.collector.where(User::getRandomNumber).eq(1);
        result.add(new Checker<>(users, collector));
        collector = check.collector.where((ComparablePath<User, Integer>) User::getRandomNumber).eq(1);
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).eq(1));
        result.add(new Checker<>(users, collector));
        users = check.expected.stream()
                .filter(it -> it.getRandomNumber() == 1 || it.getRandomNumber() == 2)
                .collect(Collectors.toList());
        collector = check.collector.where(get(User::getRandomNumber).eq(1).or(User::getRandomNumber).eq(2));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).eq(1)
                .orIf(true, root -> root.get(User::getRandomNumber).eq(2))
                .orIf(false, root -> root.get(User::getId).eq(2)));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).eq(1).or((ComparablePath<User, Integer>) User::getRandomNumber).eq(2));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).eq(1).or(Lists.of(get(User::getRandomNumber).eq(2))));
        result.add(new Checker<>(users, collector));
        users = check.expected.stream()
                .filter(it -> it.getRandomNumber() == 1 && it.isValid())
                .collect(Collectors.toList());
        collector = check.collector.where(User::getRandomNumber).eq(1).where(User::isValid).eq(true);
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(User::getRandomNumber).eq(1)
                .where((Path<User, Boolean>) User::isValid).eq(TypedExpressions.ofTrue());
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).eq(1).and(User::isValid));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).eq(1)
                .andIf(true, root -> root.get(User::isValid)));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).eq(1).and(Lists.of(get(User::isValid))));
        result.add(new Checker<>(users, collector));
        collector = check.collector
                .whereIf(true, root -> root.get(User::getRandomNumber).eq(1))
                .whereIf(true, root -> root.get(User::isValid))
                .whereIf(false, root -> root.get(User::getId).eq(2));
        result.add(new Checker<>(users, collector));

        collector = check.collector
                .where(User::getRandomNumber).eq(1)
                .whereIf(true, root -> root.get(User::isValid))
                .whereIf(false, root -> root.get(User::getId).eq(2));
        result.add(new Checker<>(users, collector));

        //
        //    B eq(ExpressionHolder<T, U> expression);
        users = check.expected.stream().filter(it -> it.getRandomNumber() == it.getId()).collect(Collectors.toList());
        collector = check.collector.where(User::getRandomNumber).eq(get(User::getId));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).eq(get(User::getId)));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where((Root<User> root) -> root.get(User::getRandomNumber).eq(root.get(User::getId)));
        result.add(new Checker<>(users, collector));
        collector = check.collector
                .whereIf(true, root -> root.get(User::getRandomNumber).eq(root.get(User::getId)))
                .whereIf(false, root -> root.get(User::getId).eq(2));
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
                    .<TypedExpression<User, Integer>>map(TypedExpressions::of)
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
        collector = check.collector.where(User::getRandomNumber).ge(TypedExpressions.of(50));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).ge(TypedExpressions.of(50)));
        result.add(new Checker<>(users, collector));

        //
        //        B gt(U value);
        users = check.expected.stream().filter(it -> it.getRandomNumber() > 50).collect(Collectors.toList());
        collector = check.collector.where(User::getRandomNumber).gt(50);
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).gt(50));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(User::getRandomNumber).gt(TypedExpressions.of(50));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).gt(TypedExpressions.of(50)));
        result.add(new Checker<>(users, collector));
        //
        //        B le(U value);
        users = check.expected.stream().filter(it -> it.getRandomNumber() <= 50).collect(Collectors.toList());
        collector = check.collector.where(User::getRandomNumber).le(50);
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).le(50));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(User::getRandomNumber).le(TypedExpressions.of(50));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).le(TypedExpressions.of(50)));
        result.add(new Checker<>(users, collector));
        //
        //        B lt(U value);
        users = check.expected.stream().filter(it -> it.getRandomNumber() < 50).collect(Collectors.toList());
        collector = check.collector.where(User::getRandomNumber).lt(50);
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).lt(50));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(User::getRandomNumber).lt(TypedExpressions.of(50));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).lt(TypedExpressions.of(50)));
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
        collector = check.collector.where(User::getRandomNumber).between(25, TypedExpressions.of(73));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).between(25, TypedExpressions.of(73)));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(User::getRandomNumber).between(TypedExpressions.of(25), 73);
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).between(TypedExpressions.of(25), 73));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(User::getRandomNumber)
                .between(TypedExpressions.of(25), TypedExpressions.of(73));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber)
                .between(TypedExpressions.of(25), TypedExpressions.of(73)));
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
        collector = check.collector.where(User::getRandomNumber).notBetween(25, TypedExpressions.of(73));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).notBetween(25, TypedExpressions.of(73)));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(User::getRandomNumber).notBetween(TypedExpressions.of(25), 73);
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).notBetween(TypedExpressions.of(25), 73));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(User::getRandomNumber)
                .notBetween(TypedExpressions.of(25), TypedExpressions.of(73));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber)
                .notBetween(TypedExpressions.of(25), TypedExpressions.of(73)));
        result.add(new Checker<>(users, collector));

        //   NumberOperator<T, U, B> add(U value);
        users = check.expected.stream().filter(it -> it.getRandomNumber() + 1 == 5).collect(Collectors.toList());
        collector = check.collector.where(User::getRandomNumber).add(1).eq(5);
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).add(1).eq(5));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(User::getRandomNumber).add(TypedExpressions.of(1)).eq(5);
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).add(TypedExpressions.of(1)).eq(5));
        result.add(new Checker<>(users, collector));

        //
        //        NumberOperator<T, U, B> subtract(U value);
        users = check.expected.stream().filter(it -> it.getRandomNumber() - 1 == 5).collect(Collectors.toList());
        collector = check.collector.where(User::getRandomNumber).subtract(1).eq(5);
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).subtract(1).eq(5));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(User::getRandomNumber).subtract(TypedExpressions.of(1)).eq(5);
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).subtract(TypedExpressions.of(1)).eq(5));
        result.add(new Checker<>(users, collector));
        //
        //        NumberOperator<T, U, B> multiply(U value);
        users = check.expected.stream().filter(it -> it.getRandomNumber() * 3 == 45).collect(Collectors.toList());
        collector = check.collector.where(User::getRandomNumber).multiply(3).eq(45);
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).multiply(3).eq(45));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(User::getRandomNumber).multiply(TypedExpressions.of(3)).eq(45);
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).multiply(TypedExpressions.of(3)).eq(45));
        result.add(new Checker<>(users, collector));
        //
        //        NumberOperator<T, U, B> divide(U value);
        users = check.expected.stream().filter(it -> it.getRandomNumber() / 3. == 12).collect(Collectors.toList());
        collector = check.collector.where(User::getRandomNumber).divide(3).eq(12);
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).divide(3).eq(12));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(User::getRandomNumber).divide(TypedExpressions.of(3)).eq(12);
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).divide(TypedExpressions.of(3)).eq(12));
        result.add(new Checker<>(users, collector));

        //
        //        NumberOperator<T, U, B> mod(U value);

        users = check.expected.stream().filter(it -> it.getRandomNumber() % 8 == 2).collect(Collectors.toList());
        collector = check.collector.where(User::getRandomNumber).mod(8).eq(2);
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).mod(8).eq(2));
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(User::getRandomNumber).mod(TypedExpressions.of(8)).eq(2);
        result.add(new Checker<>(users, collector));
        collector = check.collector.where(get(User::getRandomNumber).mod(TypedExpressions.of(8)).eq(2));
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

    private static <T, U extends Query.Collector<T>> void addTestCaseAndCheck(List<Checker<T, U>> result, Checker<T, U> checker) {
        result.add(checker);
    }

    private static <T> Stream<T> newStream(Checker<T, ?> check) {
        return check.expected.stream();
    }

    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    void queryList(Select<User> userQuery) {
        User single = userQuery
                .where(User::getId).le(10)
                .getSingle(10);
        User user = users().get(10);
        assertEquals(single, user);
        single = userQuery
                .where(User::getId).eq(10)
                .single().orElse(null);
        User user10 = null;
        for (User u : users()) {
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

        assertTrue(userQuery
                .where(User::getId).le(10)
                .single(11).isEmpty());

        Slice<User> slice = userQuery.slice(20, 10);
        assertEquals(slice.total(), users().size());
        List<User> list = users().stream()
                .skip(20)
                .limit(10)
                .toList();
        assertEquals(slice.data(), list);

        Page<User> page = userQuery.slice(new Pageable<>(3, 10));
        assertEquals(page.getTotal(), users().size());
        assertEquals(page.getList(), list);

        page = userQuery
                .orderBy(User::getId)
                .slice(new Pageable<>(3, 10));
        assertEquals(page.getTotal(), users().size());
        assertEquals(page.getList(), list);

        user = userQuery.getFirst();
        assertEquals(user, users().get(0));
        user = userQuery.getFirst(10);
        assertEquals(user, users().get(10));
        user = userQuery.first().orElse(null);
        assertEquals(user, users().get(0));
        user = userQuery.first(8).orElse(null);
        assertEquals(user, users().get(8));

    }


    @ParameterizedTest
    @ArgumentsSource(UserQueryProvider.class)
    void lock(Select<User> userQuery) {
        doInTransaction(() -> testLock(userQuery));
    }

    private static void testLock(Select<User> userQuery) {
        for (LockModeType lockModeType : LockModeType.values()) {
            try {
                User single = userQuery
                        .where(User::getId).le(10)
                        .getSingle(10, lockModeType);
                User user = users().get(10);
                assertEquals(single, user);
                single = userQuery
                        .where(User::getId).eq(10)
                        .single(lockModeType).orElse(null);
                User user10 = null;
                for (User u : users()) {
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

                assertTrue(userQuery
                        .where(User::getId).le(10)
                        .single(11, lockModeType).isEmpty());


                user = userQuery.getFirst(lockModeType);
                assertEquals(user, users().get(0));
                user = userQuery.getFirst(10, lockModeType);
                assertEquals(user, users().get(10));
                user = userQuery.first(lockModeType).orElse(null);
                assertEquals(user, users().get(0));
                user = userQuery.first(8, lockModeType).orElse(null);
                assertEquals(user, users().get(8));

                user = userQuery.where(User::getId).eq(0)
                        .requireSingle(lockModeType);
                assertEquals(user, users().get(0));

                List<User> users = userQuery.where(User::getId).eq(0)
                        .getList(0, lockModeType);
                assertEquals(users.get(0), users().get(0));
                users = userQuery.where(User::getId).eq(0)
                        .getList(lockModeType);
                assertEquals(users.get(0), users().get(0));

            } catch (Exception e) {
                log.error(lockModeType.name(), e);
            }
        }
    }

    static class Checker<T, U extends Query.Collector<T>> {
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