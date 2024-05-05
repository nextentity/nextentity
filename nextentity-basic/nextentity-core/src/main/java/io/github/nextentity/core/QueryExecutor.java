package io.github.nextentity.core;

import io.github.nextentity.core.expression.QueryStructure;
import org.jetbrains.annotations.NotNull;

import java.util.List;

public interface QueryExecutor {
    <T> List<T> getList(@NotNull QueryStructure queryStructure);
}
