package io.github.nextentity.test;

import jakarta.persistence.EntityManager;
import jakarta.persistence.EntityManagerFactory;
import jakarta.persistence.SharedCacheMode;
import jakarta.persistence.ValidationMode;
import jakarta.persistence.spi.ClassTransformer;
import jakarta.persistence.spi.PersistenceUnitInfo;
import jakarta.persistence.spi.PersistenceUnitTransactionType;
import org.hibernate.boot.model.naming.CamelCaseToUnderscoresNamingStrategy;
import org.hibernate.dialect.MySQLDialect;
import org.hibernate.jpa.HibernatePersistenceProvider;
import org.hibernate.tool.schema.Action;

import javax.sql.DataSource;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import static org.hibernate.cfg.AvailableSettings.*;

public class EntityManagers {
    private static final EntityManagerFactory factory = getEntityManagerFactory();

    private static final EntityManager ENTITY_MANAGER = doGetEntityManager();

    private EntityManagers() {
    }

    public static EntityManager getEntityManager() {
        return ENTITY_MANAGER;
    }

    private static EntityManager doGetEntityManager() {
        return factory.createEntityManager();
    }

    private static EntityManagerFactory getEntityManagerFactory() {
        DataSourceConfig config = new DataSourceConfig();
        Map<String, Object> properties = Maps.<String, Object>hashmap()
                .put(JAKARTA_JDBC_DRIVER, "com.mysql.cj.jdbc.Driver")
                .put(JAKARTA_JDBC_URL, config.getUrl())
                .put(USER, config.getUser())
                .put(PASS, config.getPassword())
                .put(DIALECT, MySQLDialect.class)
                .put(HBM2DDL_AUTO, Action.UPDATE)
                .put(SHOW_SQL, true)
                .put(FORMAT_SQL, true)
                .put(QUERY_STARTUP_CHECKING, false)
                .put(GENERATE_STATISTICS, false)
                .put(USE_REFLECTION_OPTIMIZER, false)
                .put(USE_SECOND_LEVEL_CACHE, false)
                .put(USE_QUERY_CACHE, false)
                .put(USE_STRUCTURED_CACHE, false)
                .put(STATEMENT_BATCH_SIZE, 2000)
                .put(PHYSICAL_NAMING_STRATEGY, CamelCaseToUnderscoresNamingStrategy.class)
                .build();
        return new HibernatePersistenceProvider()
                .createContainerEntityManagerFactory(info(), properties);
    }

    private static PersistenceUnitInfo info() {
        return new PersistenceUnitInfo() {
            @Override
            public String getPersistenceUnitName() {
                return "ApplicationPersistenceUnit";
            }

            @Override
            public String getPersistenceProviderClassName() {
                return "org.hibernate.jpa.HibernatePersistenceProvider";
            }

            @Override
            public PersistenceUnitTransactionType getTransactionType() {
                return PersistenceUnitTransactionType.RESOURCE_LOCAL;
            }

            @Override
            public DataSource getJtaDataSource() {
                return null;
            }

            @Override
            public DataSource getNonJtaDataSource() {
                return null;
            }

            @Override
            public List<String> getMappingFileNames() {
                return Collections.emptyList();
            }

            @Override
            public List<java.net.URL> getJarFileUrls() {
                try {
                    return Collections.list(this.getClass()
                            .getClassLoader()
                            .getResources(""));
                } catch (IOException e) {
                    throw new UncheckedIOException(e);
                }
            }

            @Override
            public java.net.URL getPersistenceUnitRootUrl() {
                return null;
            }

            @Override
            public List<String> getManagedClassNames() {
                return Collections.emptyList();
            }

            @Override
            public boolean excludeUnlistedClasses() {
                return false;
            }

            @Override
            public SharedCacheMode getSharedCacheMode() {
                return null;
            }

            @Override
            public ValidationMode getValidationMode() {
                return null;
            }

            @Override
            public Properties getProperties() {
                return new Properties();
            }

            @Override
            public String getPersistenceXMLSchemaVersion() {
                return null;
            }

            @Override
            public ClassLoader getClassLoader() {
                return null;
            }

            @Override
            public void addTransformer(ClassTransformer transformer) {

            }

            @Override
            public ClassLoader getNewTempClassLoader() {
                return null;
            }
        };
    }
}
