package io.github.nextentity.example.controller;

import io.github.nextentity.data.common.Access;
import io.github.nextentity.example.eneity.User;
import io.github.nextentity.core.domain.Page;
import io.github.nextentity.example.model.UserQuery;
import lombok.RequiredArgsConstructor;

/**
 * @author HuangChengwei
 * @since 2024-03-19 17:09
 */
@RequiredArgsConstructor
public class UserController {

    private final Access<User, Long> userAccess;

    public Page<User> getUsers(UserQuery query) {
        return userAccess.where(query.predicate()).getPage(query.pageable());
    }

    public Page<User> joinExample(UserQuery query) {
        return userAccess
                .fetch(User::getParentUser, User::getRandomUser)
                .where(query.predicate())
                .getPage(query.pageable());
    }

}
