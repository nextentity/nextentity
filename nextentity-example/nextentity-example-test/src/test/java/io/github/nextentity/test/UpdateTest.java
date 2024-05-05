package io.github.nextentity.test;

import io.github.nextentity.api.Repository;
import io.github.nextentity.api.Select;
import io.github.nextentity.core.RepositoryFactory;
import io.github.nextentity.test.db.DbConfig;
import io.github.nextentity.test.db.Transaction;
import io.github.nextentity.test.db.UserRepository;
import io.github.nextentity.test.entity.AutoGenId;
import io.github.nextentity.test.entity.User;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ArgumentsSource;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;


public class UpdateTest {

    Select<User> query(UserRepository updater) {
        return updater;
    }

    @ParameterizedTest
    @ArgumentsSource(UserUpdaterProvider.class)
    void insert(UserRepository userUpdater) {
        userUpdater.doInTransaction(() -> doInsert(userUpdater));
    }

    private void doInsert(UserRepository userUpdater) {
        List<User> existUsers = query(userUpdater).where(User::getId).in(10000000, 10000001, 10000002)
                .getList();
        if (!existUsers.isEmpty()) {
            userUpdater.delete(existUsers);
        }
        List<User> exist = query(userUpdater).where(User::getId).in(10000000, 10000001, 10000002).getList();
        assertTrue(exist.isEmpty());

        User newUser = newUser(10000000);
        userUpdater.insert(newUser);
        User single = query(userUpdater).where(User::getId).eq(10000000).getSingle();
        assertEquals(newUser, single);
        List<User> users = Arrays.asList(newUser(10000001), newUser(10000002));
        userUpdater.insert(users);
        List<User> userList = query(userUpdater).where(User::getId).in(10000001, 10000002).getList();
        assertEquals(userList, new ArrayList<>(users));
        userUpdater.delete(newUser);
        userUpdater.delete(users);
        exist = query(userUpdater).where(User::getId).in(10000000, 10000001, 10000002).getList();
        assertTrue(exist.isEmpty());
    }

    private static User newUser(int id) {
        return Users.newUser(id, "Username-" + id, new Random());
    }


    @ParameterizedTest
    @ArgumentsSource(UserUpdaterProvider.class)
    void update(UserRepository userUpdater) {
        userUpdater.doInTransaction(() -> testUpdate(userUpdater));
    }

    private void testUpdate(UserRepository userUpdater) {
        List<User> users = query(userUpdater).where(User::getId).in(1, 2, 3).getList();
        for (User user : users) {
            user.setRandomNumber(user.getRandomNumber() + 1);
        }
        userUpdater.update(users);
        assertEquals(users, query(userUpdater).where(User::getId).in(1, 2, 3).getList());

        for (User user : users) {
            user.setRandomNumber(user.getRandomNumber() + 1);
            userUpdater.update(user);
        }
        assertEquals(users, query(userUpdater).where(User::getId).in(1, 2, 3).getList());
    }

    @ParameterizedTest
    @ArgumentsSource(UserUpdaterProvider.class)
    void updateNonNullColumn(UserRepository userUpdater) {
        userUpdater.doInTransaction(() -> testUpdateNonNullColumn(userUpdater));
    }

    private void testUpdateNonNullColumn(UserRepository userUpdater) {
        List<User> users = query(userUpdater).where(User::getId).in(1, 2, 3).getList();
        List<User> users2 = new ArrayList<>(users.size());
        for (User user : users) {
            user = user.clone();
            User user2 = user.clone();
            users2.add(user2);
            int randomNumber = user.getRandomNumber() + 1;
            user.setRandomNumber(randomNumber);
            user2.setRandomNumber(randomNumber);
            user.setUsername(null);
            user.setTime(null);
            user.setPid(null);
            userUpdater.updateNonNullColumn(user);
        }
        assertEquals(users2, query(userUpdater).where(User::getId).in(1, 2, 3).getList());

    }

    @ParameterizedTest
    @ArgumentsSource(UserUpdaterProvider.class)
    public void test(UserRepository userUpdater) {
        userUpdater.doInTransaction(() -> {
            UserRepository jdbc = userUpdater.getConfig().getJdbc();
            User user = ((Select<User>) jdbc).where(User::getId).eq(10000006).getSingle();
            if (user != null) {
                jdbc.delete(user);
            }
            User entity = newUser(10000006);
            jdbc.insert(entity);
            user = ((Select<User>) jdbc).where(User::getId).eq(10000006).getSingle();
            assertEquals(entity, user);
            System.out.println(entity.getInstant());
            System.out.println(user.getInstant());
        });

        userUpdater.doInTransaction(() -> {
            UserRepository jpa = userUpdater.getConfig().getJpa();
            User user = ((Select<User>) jpa).where(User::getId).eq(10000007).getSingle();
            if (user != null) {
                jpa.delete(user);
            }
            User entity = newUser(10000007);
            jpa.insert(entity);
            user = ((Select<User>) jpa).where(User::getId).eq(10000007).getSingle();
            assertEquals(entity, user);
            System.out.println(entity.getInstant());
            System.out.println(user.getInstant());
        });
    }

    @ParameterizedTest
    @ArgumentsSource(UserUpdaterProvider.class)
    public void test2(UserRepository userUpdater) {
        User a = userUpdater.getConfig().getJdbc().where(User::getId).eq(1).getSingle();
        User b = userUpdater.getConfig().getJpa().where(User::getId).eq(1).getSingle();
        System.out.println(a);
        System.out.println(b);
    }

    @ParameterizedTest
    @ArgumentsSource(DbProvider.class)
    void testIdGenerator(DbConfig config) {
        RepositoryFactory factory = config.getJdbcFactory();
        Repository<Long, AutoGenId> entities = factory.getRepository(AutoGenId.class);
        Transaction transaction = new Transaction(config);
        transaction.doInTransaction(() -> {
            List<AutoGenId> list = Arrays.asList(new AutoGenId("a"), new AutoGenId("b"));
            entities.insert(list);
            checkId(list, entities);
        });

        transaction.doInTransaction(() -> {
            List<AutoGenId> list = Arrays.asList(new AutoGenId(), new AutoGenId());
            entities.insert(list);
            checkId(list, entities);
            entities.update(list);
            entities.update(list.get(0));
        });
    }

    private static void checkId(List<AutoGenId> list, Repository<Long, AutoGenId> entities) {
        for (AutoGenId autoGenId : list) {
            assertNotNull(autoGenId.getId());
        }
        List<Long> ids = list.stream().map(AutoGenId::getId).collect(Collectors.toList());
        Map<Long, AutoGenId> map = entities.getMap(ids);
        for (AutoGenId autoGenId : list) {
            AutoGenId a = map.get(autoGenId.getId());
            assertEquals(autoGenId, a);
        }
    }

}
