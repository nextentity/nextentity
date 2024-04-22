package io.github.nextentity.test.entity;

import io.github.nextentity.core.annotaion.SubSelect;
import lombok.Data;

import javax.persistence.Id;

@Data
@SubSelect("select u.username as username, max(u.random_number) as max_random_number, count( u.id ) as count from `user` u group by u.username")
public class UserSummaryMysql {
    @Id
    private String username;
    private Integer maxRandomNumber;
    private Long count;
}