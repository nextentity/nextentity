package io.github.nextentity.test.entity;

import lombok.AccessLevel;
import lombok.Getter;

import javax.persistence.MappedSuperclass;
import javax.persistence.Version;

@MappedSuperclass
public class EnableOptimisticLock {

    @Version
    @Getter(AccessLevel.PRIVATE)
    private int optLock;

}
