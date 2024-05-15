package io.github.nextentity.jpa;

import io.github.nextentity.api.model.LockModeType;

public class LockModeTypeAdapter {

    public static javax.persistence.LockModeType of(LockModeType lockModeType) {
        return lockModeType == null ? null : javax.persistence.LockModeType.valueOf(lockModeType.name());
    }

}
