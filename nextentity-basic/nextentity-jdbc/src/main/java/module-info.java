module nextentity.jdbc {
    requires static lombok;
    requires static org.jetbrains.annotations;

    requires java.sql;
    requires nextentity.core;
    requires org.slf4j;

    exports io.github.nextentity.jdbc;
}