package io.github.nextentity.jpa;

import io.github.nextentity.core.api.LockModeType;

public class LockModeTypeAdapter {

    public static jakarta.persistence.LockModeType of(LockModeType lockModeType) {
        return lockModeType == null ? null : jakarta.persistence.LockModeType.valueOf(lockModeType.name());
    }

}
