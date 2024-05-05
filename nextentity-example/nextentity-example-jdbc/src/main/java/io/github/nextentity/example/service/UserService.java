package io.github.nextentity.example.service;

import io.github.nextentity.api.Repository;
import io.github.nextentity.api.model.Page;
import io.github.nextentity.api.model.Pageable;
import io.github.nextentity.example.eneity.User;
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

    private final Repository<Long, User> userRepository;

    public void updateUser(User user) {
        userRepository.update(user);
    }

    public List<User> getByUsername(String username) {
        return userRepository.where(User::getUsername).eq(username).getList();
    }

    public User updateRandomNumber(long userId) {
        User user = userRepository.get(userId);
        return updateRandomNumber(user);
    }

    public User updateRandomNumber(User user) {
        if (user != null) {
            user.setRandomNumber(ThreadLocalRandom.current().nextInt(20));
            userRepository.update(user);
        }
        return user;
    }

    public Page<User> page(String username, Pageable pageable) {
        return userRepository
                .where(User::getUsername).eqIfNotNull(username)
                .where(User::getUsername).eqIfNotNull(username)
                .getPage(pageable);
    }

    /**
     * 投影查询示例
     */
    public Page<UsernameGender> usernameGenderPage(String username, Pageable pageable) {
        return userRepository
                .select(UsernameGender.class)
                .where(User::getUsername).eqIfNotNull(username)
                .getPage(pageable);
    }

    /**
     * 接口投影查询示例
     */
    public Page<IUsernameGender> iUsernameGenderPage(String username, Pageable pageable) {
        return userRepository
                .select(IUsernameGender.class)
                .where(User::getUsername).eqIfNotNull(username)
                .getPage(pageable);
    }
}
