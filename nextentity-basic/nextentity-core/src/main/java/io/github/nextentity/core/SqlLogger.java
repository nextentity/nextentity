package io.github.nextentity.core;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class SqlLogger {

    public static final Logger log = LoggerFactory.getLogger("io.github.nextentity.sql");

    public static void debug(String s) {
        log.debug(s);
    }

    public static void debug(String s, Object o) {
        log.debug(s, o);
    }

}
