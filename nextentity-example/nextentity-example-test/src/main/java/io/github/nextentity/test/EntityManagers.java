// package io.github.nextentity.test;
//
// import io.github.nextentity.core.util.Maps;
// import io.github.nextentity.test.db.SqlServer;
// import jakarta.persistence.EntityManager;
// import jakarta.persistence.EntityManagerFactory;
// import org.hibernate.boot.model.naming.CamelCaseToUnderscoresNamingStrategy;
// import org.hibernate.dialect.MySQLDialect;
// import org.hibernate.jpa.HibernatePersistenceProvider;
// import org.hibernate.tool.schema.Action;
//
// import java.util.Map;
//
// import static org.hibernate.cfg.AvailableSettings.*;
//
// public class EntityManagers {
//     private static final EntityManagerFactory factory = getEntityManagerFactory();
//
//     private static final EntityManager ENTITY_MANAGER = doGetEntityManager();
//
//     private EntityManagers() {
//     }
//
//     public static EntityManager getEntityManager() {
//         return SqlServer.getEntityManager();
//     }
//
//     private static EntityManager doGetEntityManager() {
//         return factory.createEntityManager();
//     }
//
//     private static EntityManagerFactory getEntityManagerFactory() {
//         DataSourceConfig config = new DataSourceConfig();
//         Map<String, Object> properties = Maps.<String, Object>hashmap()
//                 .put(JAKARTA_JDBC_DRIVER, "com.mysql.cj.jdbc.Driver")
//                 .put(JAKARTA_JDBC_URL, config.getUrl())
//                 .put(USER, config.getUser())
//                 .put(PASS, config.getPassword())
//                 .put(DIALECT, MySQLDialect.class)
//                 .put(HBM2DDL_AUTO, Action.UPDATE)
//                 .put(SHOW_SQL, true)
//                 .put(FORMAT_SQL, true)
//                 .put(QUERY_STARTUP_CHECKING, false)
//                 .put(GENERATE_STATISTICS, false)
//                 .put(USE_REFLECTION_OPTIMIZER, false)
//                 .put(USE_SECOND_LEVEL_CACHE, false)
//                 .put(USE_QUERY_CACHE, false)
//                 .put(USE_STRUCTURED_CACHE, false)
//                 .put(STATEMENT_BATCH_SIZE, 2000)
//                 .put(PHYSICAL_NAMING_STRATEGY, CamelCaseToUnderscoresNamingStrategy.class)
//                 .build();
//         return new HibernatePersistenceProvider()
//                 .createContainerEntityManagerFactory(new HibernateUnitInfo(), properties);
//     }
// }
