package io.github.nextentity.test.db;

import io.github.nextentity.core.util.ImmutableList;

import java.util.List;

/**
 * @author HuangChengwei
 * @since 2024-04-10 15:55
 */
public interface DbConfigs {

    DbConfig MYSQL = new Mysql().getConfig();
    DbConfig SQLSERVER = MYSQL;
    List<DbConfig> CONFIGS = ImmutableList.of(MYSQL);

}
