package io.github.nextentity.example.model;

import io.github.nextentity.api.TypedExpression;
import io.github.nextentity.core.util.Paths;
import io.github.nextentity.example.eneity.Gender;
import io.github.nextentity.example.eneity.User;
import lombok.Data;

/**
 * @author HuangChengwei
 * @since 2024-03-19 16:52
 */
@Data
public class UserQuery implements PageablePredicate<User> {

    private String username;
    private String parentUsername;
    private Gender gender;
    private Integer page;
    private Integer size;

    @Override
    public TypedExpression<User, Boolean> predicate() {
        return Paths.get(User::getUsername).eqIfNotNull(username)
                .and(User::getGender).eqIfNotNull(gender)
                .and(User::getParentUser).get(User::getUsername).eqIfNotNull(parentUsername);
    }
}
