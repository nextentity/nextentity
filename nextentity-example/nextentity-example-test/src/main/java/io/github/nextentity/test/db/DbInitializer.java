package io.github.nextentity.test.db;

import io.github.nextentity.test.Users;
import io.github.nextentity.test.entity.User;
import lombok.extern.slf4j.Slf4j;

import java.sql.Connection;
import java.sql.SQLException;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * @author HuangChengwei
 * @since 2024-04-10 15:43
 */
@Slf4j
public class DbInitializer extends Transaction {
    List<User> allUsers;

    public DbInitializer(DbConfig config) {
        this.config = config;
    }

    public synchronized List<User> initialize() {
        doInTransaction(connection -> {
            try {
                UserRepository query = config.getJdbc();
                // resetData(connection, query);
                allUsers = queryAllUsers(query);
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        });
        return allUsers;
    }

    private void resetData(Connection connection, UserRepository query) throws SQLException {
        String sql = config.getSetPidNullSql();
        //noinspection SqlSourceToSinkFlow
        connection.createStatement().executeUpdate(sql);
        query.delete(queryAllUsers(query));
        query.insert(Users.getUsers());
    }

    private List<User> queryAllUsers(UserRepository query) {
        List<User> list = query.orderBy(User::getId).asc().getList();
        Map<Integer, User> map = list.stream().collect(Collectors.toMap(User::getId, Function.identity()));
        for (User user : list) {
            Integer pid = user.getPid();
            User p = map.get(pid);
            user.setParentUser(p);
            user.setRandomUser(map.get(user.getRandomNumber()));
            user.setTestUser(map.get(user.getTestInteger()));
        }
        return list;
    }


}
