package io.github.nextentity.example.service;

import io.github.nextentity.data.common.Access;
import io.github.nextentity.example.eneity.User;
import io.github.nextentity.example.model.Page;
import io.github.nextentity.example.model.Pageable;
import io.github.nextentity.example.projection.IUsernameGender;
import io.github.nextentity.example.projection.UsernameGender;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.concurrent.ThreadLocalRandom;

/**
 * @author HuangChengwei
 * @since 2024-03-19 13:55
 */
@Service
@RequiredArgsConstructor
public class UserService {

    private final Access<User, Long> userAccess;

    public List<User> getByUsername(String username) {
        return userAccess.where(User::getUsername).eq(username).getList();
    }

    public User updateRandomNumber(long userId) {
        User user = userAccess.get(userId);
        return updateRandomNumber(user);
    }

    public User updateRandomNumber(User user) {
        if (user != null) {
            user.setRandomNumber(ThreadLocalRandom.current().nextInt(20));
            userAccess.update(user);
        }
        return user;
    }

    public Page<User> page(String username, Pageable<User> pageable) {
        return userAccess
                .where(User::getUsername).eqIfNotNull(username)
                .where(User::getUsername).eqIfNotNull(username)
                .slice(pageable);
    }

    /**
     * 投影查询示例
     */
    public Page<UsernameGender> usernameGenderPage(String username, Pageable<UsernameGender> pageable) {
        return userAccess
                .select(UsernameGender.class)
                .where(User::getUsername).eqIfNotNull(username)
                .slice(pageable);
    }


    /**
     * 接口投影查询示例
     */
    public Page<IUsernameGender> iUsernameGenderPage(String username, Pageable<IUsernameGender> pageable) {
        return userAccess
                .select(IUsernameGender.class)
                .where(User::getUsername).eqIfNotNull(username)
                .slice(pageable);
    }
}
