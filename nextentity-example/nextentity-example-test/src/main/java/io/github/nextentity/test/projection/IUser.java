package io.github.nextentity.test.projection;

import lombok.Data;

public interface IUser {

    int getId();

    int getRandomNumber();

    String getUsername();

    U getParentUser();

    @Data
    class U {
        private int id;
        private int randomNumber;
        private Object test;
        private String username;
        private U parentUser;
    }

}
