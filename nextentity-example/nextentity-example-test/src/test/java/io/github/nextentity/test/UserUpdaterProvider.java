package io.github.nextentity.test;

import io.github.nextentity.test.db.DbConfigs;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.ArgumentsProvider;

import java.util.stream.Stream;

public class UserUpdaterProvider implements ArgumentsProvider {

    @Override
    public Stream<? extends Arguments> provideArguments(ExtensionContext extensionContext) {
        return DbConfigs.CONFIGS.stream()
                .peek(dbConfig -> dbConfig.getEntityManager().clear())
                .flatMap(dbConfig -> Stream.of(dbConfig.getJdbc(), dbConfig.getJpa()))
                .map(Arguments::of);
    }

}
