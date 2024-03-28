package io.github.nextentity.test;

import io.github.nextentity.core.PathReference;
import io.github.nextentity.test.entity.User;

public class TestPathReference {

    public static void main(String[] args) {
        PathReference reference = PathReference.of(User::getId);
        System.out.println(reference);
    }

}
