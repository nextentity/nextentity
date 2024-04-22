package io.github.nextentity.test.projection;

import lombok.Data;

public interface IUser {

    int getId();

    int getRandomNumber();

    String getUsername();

    U getParentUser();


    @Data
    class U {
        int id;
        int randomNumber;
        String username;
    }

}
