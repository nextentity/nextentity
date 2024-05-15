package io.github.nextentity.example.controller;

import io.github.nextentity.api.model.Page;
import io.github.nextentity.example.eneity.Gender;
import io.github.nextentity.example.eneity.User;
import io.github.nextentity.example.model.UserQuery;
import io.github.nextentity.example.model.UserQuery2;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

/**
 * @author HuangChengwei
 * @since 2024-03-30 9:45
 */
@SpringBootTest
class UserControllerTest {

    @Autowired
    UserController userController;

    @Test
    void getUsers() {
        UserQuery query = new UserQuery();
        query.setUsername("test");
        query.setParentUsername("p");
        query.setGender(Gender.MALE);
        query.setPage(2);
        query.setSize(20);

        Page<User> users = userController.getUsers(query);
        System.out.println(users);
    }

    @Test
    void joinExample() {
        UserQuery2 query = new UserQuery2();
        query.setUsername("test");
        query.setParentUsername("p");
        query.setGender(Gender.MALE);
        query.setPage(2);
        query.setSize(20);

        Page<User> userPage = userController.joinExample(query);
        System.out.println(userPage);
    }
}