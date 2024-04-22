package io.github.nextentity.test;

import io.github.nextentity.core.PathReference;
import lombok.Data;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class PathReferenceTest {
    @Test
    void test() {
        PathReference pathReference = PathReference.of(TestLastClass::getName);
        Assertions.assertEquals(pathReference.getFieldName(), "name");
        Assertions.assertEquals(pathReference.getReturnType(), String.class);
        Assertions.assertEquals(pathReference.getEntityType(), TestLastClass.class);
    }

    @Data
    public static class TestLastClass {
        String name;
    }

}
