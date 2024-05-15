package io.github.nextentity.test.entity;


import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import lombok.Data;

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
