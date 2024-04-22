package io.github.nextentity.test;

import io.github.nextentity.test.db.DbConfigs;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.ArgumentsProvider;

import java.util.stream.Stream;

/**
 * @author HuangChengwei
 * @since 2024/4/15 上午11:22
 */
public class DbProvider implements ArgumentsProvider {
    @Override
    public Stream<? extends Arguments> provideArguments(ExtensionContext extensionContext) {
        return Stream.of(Arguments.of(DbConfigs.MYSQL), Arguments.of(DbConfigs.SQLSERVER));
    }
}
