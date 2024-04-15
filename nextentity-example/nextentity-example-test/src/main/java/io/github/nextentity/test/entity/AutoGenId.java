package io.github.nextentity.test.entity;


import jakarta.persistence.Entity;
import lombok.Data;


import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;

@Entity
@Data
public class AutoGenId {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    Long id;
    String name;

    public AutoGenId() {
    }

    public AutoGenId(String name) {
        this.name = name;
    }
}
