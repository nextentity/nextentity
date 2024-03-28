package io.github.nextentity.example.controller;

import io.github.nextentity.data.common.Access;
import io.github.nextentity.example.eneity.User;
import io.github.nextentity.example.model.Page;
import io.github.nextentity.example.model.UserQuery;
import io.github.nextentity.example.model.UserQuery2;
import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * @author HuangChengwei
 * @since 2024-03-19 17:09
 */
@RestController
@RequiredArgsConstructor
public class UserController {

    private final Access<User, Long> userAccess;

    @GetMapping("/user/list")
    public Page<User> getUsers(UserQuery query) {
        return userAccess.where(query.predicate()).slice(query.pageable());
    }

    @GetMapping("/user/list/join-example")
    public Page<User> joinExample(UserQuery2 query) {
        return userAccess
                .fetch(User::getParentUser, User::getRandomUser)
                .where(query.predicate())
                .slice(query.pageable());
    }

}
