package io.github.nextentity.example.eneity;

import io.github.nextentity.api.TypedExpression.EntityPathExpression;
import io.github.nextentity.api.TypedExpression.NumberPathExpression;
import io.github.nextentity.api.TypedExpression.PathExpression;
import io.github.nextentity.api.TypedExpression.StringPathExpression;
import io.github.nextentity.core.Persistable;
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

import static io.github.nextentity.core.util.Paths.get;
import static jakarta.persistence.ConstraintMode.NO_CONSTRAINT;

@SuppressWarnings("JpaDataSourceORMInspection")
@Entity
@ToString
@Getter
@Setter
public class User implements Persistable<Long> {

    public static StringPathExpression<User> Username = get(User::getUsername);
    public static EntityPathExpression<User, User> ParentUser = get(User::getParentUser);
    public static PathExpression<User, Gender> Gender = get(User::getGender);
    public static NumberPathExpression<User, Long> Pid = get(User::getPid);

    @Id
    private Long id;

    private Integer randomNumber;

    private String username;

    private Date time;

    private Long pid;

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
