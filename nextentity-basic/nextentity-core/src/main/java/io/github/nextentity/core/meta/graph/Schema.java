package io.github.nextentity.core.meta.graph;

import java.util.Collection;

public interface Schema extends Graph {
    Collection<? extends Property> properties();

    default Property getProperty(final String name) {
        return properties().stream()
                .filter(i -> i.name().equals(name))
                .findFirst()
                .orElse(null);
    }

}
