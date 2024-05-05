module nextentity.core {
    requires static lombok;
    requires static org.jetbrains.annotations;

    requires java.desktop;
    requires org.slf4j;
    requires java.sql;
    requires nextentity.api;

    exports io.github.nextentity.core;
    exports io.github.nextentity.core.exception;
    exports io.github.nextentity.core.meta;
    exports io.github.nextentity.core.reflect;
    exports io.github.nextentity.core.util;
    exports io.github.nextentity.core.converter;
    exports io.github.nextentity.core.expression;
    exports io.github.nextentity.core.annotaion;
    exports io.github.nextentity.core.reflect.schema;
    exports io.github.nextentity.core.expression.impl;
}