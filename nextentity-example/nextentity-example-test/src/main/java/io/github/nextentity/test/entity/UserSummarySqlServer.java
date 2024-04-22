package io.github.nextentity.test.entity;

import io.github.nextentity.core.annotaion.SubSelect;
import lombok.Data;

import javax.persistence.Id;

@Data
@SubSelect("SELECT u.username AS username, max(u.random_number) AS max_random_number, count( u.id ) AS count from [user] u GROUP BY u.username")
public class UserSummarySqlServer {
    @Id
    private String username;
    private Integer maxRandomNumber;
    private Long count;
}