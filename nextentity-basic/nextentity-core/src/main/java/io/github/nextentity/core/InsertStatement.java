package io.github.nextentity.core;

import lombok.Getter;

import java.util.List;

/**
 * @author HuangChengwei
 * @since 2024/4/16 上午11:40
 */
@Getter
public class InsertStatement extends SqlStatement {

    private final Iterable<?> entities;
    private final boolean returnGeneratedKeys;

    public InsertStatement(String sql, List<List<?>> parameters, Iterable<?> entities, boolean returnGeneratedKeys) {
        super(sql, parameters);
        this.entities = entities;
        this.returnGeneratedKeys = returnGeneratedKeys;
    }

}
