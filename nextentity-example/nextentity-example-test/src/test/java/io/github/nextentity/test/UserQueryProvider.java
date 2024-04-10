package io.github.nextentity.test;

import io.github.nextentity.core.api.Entities;
import io.github.nextentity.test.db.DbConfig;
import io.github.nextentity.test.db.DbConfigs;
import io.github.nextentity.test.db.UserEntities;
import io.github.nextentity.test.entity.User;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.ArgumentsProvider;

import java.util.stream.Stream;

@Slf4j
public class UserQueryProvider implements ArgumentsProvider {

    @Override
    public Stream<? extends Arguments> provideArguments(ExtensionContext extensionContext) {
        return DbConfigs.configs.stream()
                .flatMap(UserQueryProvider::getArguments)
                .map(Arguments::of);
    }

    private static Stream<UserEntities> getArguments(DbConfig config) {
        return config.getEntitiesFactories().stream()
                .map(it -> {
                    Entities<Integer, User> entities = it.getEntities(User.class);
                    return new UserEntities(entities, config);
                });
    }


}
