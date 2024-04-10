package io.github.nextentity.test;

import io.github.nextentity.core.api.Query.Select;
import io.github.nextentity.test.db.UserEntities;
import io.github.nextentity.test.entity.User;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ArgumentsSource;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;


public class UpdateTest {

    Select<User> query(UserEntities updater) {
        return updater;
    }

    @ParameterizedTest
    @ArgumentsSource(UserUpdaterProvider.class)
    void insert(UserEntities userUpdater) {
        userUpdater.doInTransaction(() -> doInsert(userUpdater));
    }

    private void doInsert(UserEntities userUpdater) {
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
    void update(UserEntities userUpdater) {
        userUpdater.doInTransaction(() -> testUpdate(userUpdater));
    }

    private void testUpdate(UserEntities userUpdater) {
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
    void updateNonNullColumn(UserEntities userUpdater) {
        userUpdater.doInTransaction(() -> testUpdateNonNullColumn(userUpdater));
    }

    private void testUpdateNonNullColumn(UserEntities userUpdater) {
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
            user = userUpdater.updateNonNullColumn(user);
        }
        assertEquals(users2, query(userUpdater).where(User::getId).in(1, 2, 3).getList());

    }

    @ParameterizedTest
    @ArgumentsSource(UserUpdaterProvider.class)
    public void test(UserEntities userUpdater) {
        userUpdater.doInTransaction(() -> {
            UserEntities jdbc = userUpdater.getConfig().getJdbc();
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
            UserEntities jpa = userUpdater.getConfig().getJpa();
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
    public void test2(UserEntities userUpdater) {
        User a = userUpdater.getConfig().getJdbc().where(User::getId).eq(1).getSingle();
        User b = userUpdater.getConfig().getJpa().where(User::getId).eq(1).getSingle();
        System.out.println(a);
        System.out.println(b);
    }
}
