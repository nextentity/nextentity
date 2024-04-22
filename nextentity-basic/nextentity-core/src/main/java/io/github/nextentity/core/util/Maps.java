package io.github.nextentity.core.util;

import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

public class Maps {

    public static <K, V> Builder<HashMap<K, V>, K, V> hashmap() {
        return builder(new HashMap<>());
    }

    @NotNull
    private static <MAP extends HashMap<K, V>, K, V> Builder<MAP, K, V> builder(MAP map) {
        return new Builder<>(map);
    }

    public static class Builder<MAP extends Map<K, V>, K, V> {
        private final MAP map;

        public Builder(MAP map) {
            this.map = map;
        }

        public Builder<MAP, K, V> put(K key, V value) {
            map.put(key, value);
            return this;
        }

        public Builder<MAP, K, V> put(Collection<K> keys, Collection<V> values) {
            if (keys.size() != values.size()) {
                throw new IllegalArgumentException();
            }
            Iterator<K> itk = keys.iterator();
            Iterator<V> itv = values.iterator();
            while (itk.hasNext()) {
                put(itk.next(), itv.next());
            }
            return this;
        }

        public MAP build() {
            return map;
        }
    }

}
