package io.github.nextentity.core.api;

import java.io.Serializable;
import java.util.List;

public interface QueryStructure extends Serializable {

    Selection select();

    From from();

    Expression where();

    List<? extends Expression> groupBy();

    List<? extends Order<?>> orderBy();

    Expression having();

    Integer offset();

    Integer limit();

    LockModeType lockType();

    List<? extends Column> fetch();
}
