module nextentity.meta.jpa {
    requires static lombok;
    requires static org.slf4j;

    requires nextentity.core;
    requires jakarta.persistence;

    exports io.github.nextentity.meta.jpa;
}