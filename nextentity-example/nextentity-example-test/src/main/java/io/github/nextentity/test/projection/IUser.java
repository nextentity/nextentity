package io.github.nextentity.test.projection;

public interface IUser {

    int getId();

    int getRandomNumber();

    String getUsername();

    U getParentUser();

    record U(int id, int randomNumber, String username, U parentUser) {
    }

}
