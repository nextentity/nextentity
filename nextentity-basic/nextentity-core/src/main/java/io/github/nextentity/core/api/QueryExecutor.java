package io.github.nextentity.core.api;

import org.jetbrains.annotations.NotNull;

import java.util.List;

@FunctionalInterface
public interface QueryExecutor {

    <T> List<T> getList(@NotNull QueryStructure queryStructure);

}
