package io.github.nextentity.test.projection;

import io.github.nextentity.core.meta.EntityAttribute;
import io.github.nextentity.test.entity.User;
import lombok.Data;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@Data
public class UserModel implements UserInterface {

    private int id;

    private int randomNumber;

    private String username;

    private Integer pid;

    private boolean valid;

    @EntityAttribute("parentUser.username")
    private String parentUsername;

    public UserModel(User user) {

        id = user.getId();
        randomNumber = user.getRandomNumber();
        username = user.getUsername();
        pid = user.getPid();
        valid = user.isValid();
        if (user.getParentUser() != null) {
            parentUsername = user.getParentUser().getUsername();
        }

    }
}
