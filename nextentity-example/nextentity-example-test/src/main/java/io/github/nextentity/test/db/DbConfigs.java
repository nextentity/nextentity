package io.github.nextentity.test.db;

import io.github.nextentity.core.util.Lists;

import java.util.List;

/**
 * @author HuangChengwei
 * @since 2024-04-10 15:55
 */
public interface DbConfigs {

    List<DbConfig> configs = Lists.of(new Mysql().getConfig(), new SqlServer().getConfig());

}
