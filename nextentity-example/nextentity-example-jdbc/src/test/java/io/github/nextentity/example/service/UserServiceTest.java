package io.github.nextentity.example.service;

import io.github.nextentity.data.common.Access;
import io.github.nextentity.example.eneity.User;
import io.github.nextentity.example.model.Page;
import io.github.nextentity.example.model.Pageable;
import io.github.nextentity.example.projection.IUsernameGender;
import io.github.nextentity.example.projection.UsernameGender;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.List;

/**
 * @author HuangChengwei
 * @since 2024-03-19 14:01
 */
@SpringBootTest
class UserServiceTest {

    @Autowired
    UserService userService;
    @Autowired
    Access<User, Long> userAccess;

    @Test
    void getByUsername() {
        User first = userAccess.getFirst();
        if (first != null) {
            List<User> users = userService.getByUsername(first.getUsername());
            Assertions.assertFalse(users.isEmpty());
            for (User user : users) {
                Assertions.assertEquals(user.getUsername().toLowerCase(), first.getUsername().toLowerCase());
            }
        }
    }

    @Test
    void updateRandomNumber() {
        User first = userAccess.getFirst();
        User updated = userService.updateRandomNumber(first.getRandomNumber());
        Assertions.assertNotEquals(first.getOptLock(), updated.getOptLock());
    }

    @Test
    void page() {
        Page<User> page = userService.page(null, Pageable.of(1, 12));
        System.out.println(page);
        page = userService.page("Marjorie Minnie", Pageable.of(1, 12));
        System.out.println(page);
    }


    @Test
    void usernameGenderPage() {
        Page<UsernameGender> page = userService.usernameGenderPage(null, Pageable.of(1, 12));
        System.out.println(page);
        page = userService.usernameGenderPage("Marjorie Minnie", Pageable.of(1, 12));
        System.out.println(page);

    }

    @Test
    void iUsernameGenderPage() {
        IUsernameGender first = userAccess.select(IUsernameGender.class).getFirst();
        IUsernameGender first2 = userAccess.select(IUsernameGender.class).getFirst();
        System.out.println(first2.equals(first));


        Page<IUsernameGender> page = userService.iUsernameGenderPage(null, Pageable.of(1, 12));
        System.out.println(toString(page));
        page = userService.iUsernameGenderPage("Marjorie Minnie", Pageable.of(1, 12));
        System.out.println(toString(page));
    }

    private String toString(Object page) {
        return String.valueOf(page);
    }
}