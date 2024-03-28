package io.github.nextentity.example.eneity;

import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.ForeignKey;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Version;
import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Date;

import static jakarta.persistence.ConstraintMode.NO_CONSTRAINT;

@SuppressWarnings("JpaDataSourceORMInspection")
@Entity
@ToString
@Getter
@Setter
public class User {

    @Id
    private Long id;

    private Integer randomNumber;

    private String username;

    private Date time;

    private Integer pid;

    private Double timestamp;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "pid", insertable = false, updatable = false)
    @ToString.Exclude
    private User parentUser;

    private boolean valid;

    private Gender gender;

    private Date instant;

    private Long testLong;

    private Integer testInteger;

    private LocalDate testLocalDate;

    private LocalDateTime testLocalDateTime;


    @Version
    private int optLock;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "randomNumber", updatable = false, insertable = false, foreignKey = @ForeignKey(NO_CONSTRAINT))
    @ToString.Exclude
    private User randomUser;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "testInteger", updatable = false, insertable = false, foreignKey = @ForeignKey(NO_CONSTRAINT))
    @ToString.Exclude
    private User testUser;

    public User() {
    }

}
