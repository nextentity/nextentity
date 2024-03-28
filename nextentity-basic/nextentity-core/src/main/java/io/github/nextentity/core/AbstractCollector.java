package io.github.nextentity.core;

import io.github.nextentity.core.api.Lists;
import io.github.nextentity.core.api.Query.Collector;
import io.github.nextentity.core.api.Slice;
import io.github.nextentity.core.api.Sliceable;
import io.github.nextentity.core.QueryStructures.SliceImpl;

import java.util.List;

public interface AbstractCollector<T> extends Collector<T> {

    @Override
    default Slice<T> slice(int offset, int limit) {
        long count = count();
        if (count <= offset) {
            return new SliceImpl<>(Lists.of(), count, offset, limit);
        } else {
            List<T> list = getList(offset, limit);
            return new SliceImpl<>(list, count, offset, limit);
        }
    }

    @Override
    default <R> R slice(Sliceable<T, R> sliceable) {
        long count = count();
        if (count <= sliceable.offset()) {
            return sliceable.collect(Lists.of(), count);
        } else {
            List<T> list = getList(sliceable.offset(), sliceable.limit());
            return sliceable.collect(list, count);
        }
    }

}
