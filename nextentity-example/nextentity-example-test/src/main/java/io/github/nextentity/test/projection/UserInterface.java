package io.github.nextentity.test.projection;

import com.fasterxml.jackson.core.type.TypeReference;
import io.github.nextentity.core.annotaion.EntityAttribute;
import io.github.nextentity.test.JsonSerializablePredicateValueTest;

import java.util.Map;

public interface UserInterface {

    int getId();

    int getRandomNumber();

    String getUsername();

    Integer getPid();

    boolean isValid();

    @EntityAttribute("parentUser.username")
    String getParentUsername();

    default Map<String, Object> asMap() {
        return JsonSerializablePredicateValueTest.mapper
                .convertValue(this, new TypeReference<Map<String, Object>>() {
                });
    }
}
