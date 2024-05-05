package io.github.nextentity.test;

import io.github.nextentity.api.Repository;
import io.github.nextentity.test.db.DbConfig;
import io.github.nextentity.test.db.DbConfigs;
import io.github.nextentity.test.db.UserRepository;
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
        return DbConfigs.CONFIGS.stream()
                .peek(dbConfig -> dbConfig.getEntityManager().clear())
                .flatMap(UserQueryProvider::getArguments)
                .map(Arguments::of);
    }

    private static Stream<UserRepository> getArguments(DbConfig config) {
        return config.getEntitiesFactories().stream()
                .map(it -> {
                    Repository<Integer, User> entities = it.getRepository(User.class);
                    return new UserRepository(entities, config);
                });
    }


}
