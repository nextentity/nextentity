package io.github.nextentity.data.jdbc;

import io.github.nextentity.jdbc.ConnectionProvider;
import org.springframework.jdbc.core.JdbcTemplate;

/**
 * @author HuangChengwei
 * @since 2024-04-09 8:12
 */
public class JdbcConnectionProvider implements ConnectionProvider {
    private final JdbcTemplate jdbcTemplate;

    public JdbcConnectionProvider(JdbcTemplate jdbcTemplate) {
        this.jdbcTemplate = jdbcTemplate;
    }

    @Override
    public <T> T execute(ConnectionCallback<T> action) {
        return jdbcTemplate.execute(action::doInConnection);
    }

}
