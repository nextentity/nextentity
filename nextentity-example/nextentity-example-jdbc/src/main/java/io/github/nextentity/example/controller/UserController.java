package io.github.nextentity.example.controller;

import io.github.nextentity.api.Repository;
import io.github.nextentity.api.model.Page;
import io.github.nextentity.example.eneity.User;
import io.github.nextentity.example.model.UserQuery;
import io.github.nextentity.example.model.UserQuery2;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Controller;

/**
 * @author HuangChengwei
 * @since 2024-03-19 17:09
 */
@Controller
@RequiredArgsConstructor
public class UserController {

    private final Repository<Long, User> userRepository;

    public Page<User> getUsers(UserQuery query) {
        return userRepository.where(query.predicate()).getPage(query.pageable());
    }

    public Page<User> joinExample(UserQuery2 query) {
        return userRepository
                .fetch(User::getParentUser, User::getRandomUser)
                .where(query.predicate())
                .getPage(query.pageable());
    }

}
