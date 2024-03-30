package io.github.nextentity.example.projection;

import io.github.nextentity.example.eneity.Gender;
import lombok.Data;

/**
 * @author HuangChengwei
 * @since 2024-03-22 10:23
 */
@Data
public class UsernameGender {
    Long id;
    String username;
    Gender gender;
}
