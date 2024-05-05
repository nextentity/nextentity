package io.github.nextentity.core;

import io.github.nextentity.api.Collector;
import io.github.nextentity.api.SubQueryBuilder;
import io.github.nextentity.api.model.LockModeType;
import io.github.nextentity.api.model.Page;
import io.github.nextentity.api.model.Pageable;
import io.github.nextentity.api.model.Slice;
import io.github.nextentity.api.model.Sliceable;
import io.github.nextentity.core.expression.impl.ExpressionImpls.SliceImpl;
import io.github.nextentity.core.util.ImmutableList;
import org.jetbrains.annotations.NotNull;

import java.util.List;
import java.util.Objects;
import java.util.function.Function;

public interface AbstractCollector<T> extends Collector<T> {

    @Override
    default Slice<T> slice(int offset, int limit) {
        long count = count();
        if (count <= offset) {
            return new SliceImpl<>(ImmutableList.of(), count, offset, limit);
        } else {
            List<T> list = getList(offset, limit);
            return new SliceImpl<>(list, count, offset, limit);
        }
    }

    @Override
    default <R> R slice(Sliceable<T, R> sliceable) {
        long count = count();
        if (count <= sliceable.offset()) {
            return sliceable.collect(ImmutableList.of(), count);
        } else {
            List<T> list = getList(sliceable.offset(), sliceable.limit());
            return sliceable.collect(list, count);
        }
    }

    @Override
    default Page<T> getPage(@NotNull Pageable pageable) {
        long count = count();
        List<T> list = count <= pageable.offset()
                ? ImmutableList.of()
                : getList(pageable.offset(), pageable.size());
        return Pages.page(list, count);
    }

    @Override
    default <R> Collector<R> map(Function<? super T, ? extends R> mapper) {
        return new MappedCollector<>(this, mapper);
    }

    class MappedCollector<T, R> implements AbstractCollector<R> {

        private final Collector<T> collector;
        private final Function<? super T, ? extends R> mapper;

        public MappedCollector(Collector<T> collector, Function<? super T, ? extends R> mapper) {
            this.collector = Objects.requireNonNull(collector);
            this.mapper = Objects.requireNonNull(mapper);
        }

        @Override
        public long count() {
            return collector.count();
        }

        @Override
        public List<R> getList(int offset, int maxResult, LockModeType lockModeType) {
            List<T> list = collector.getList(offset, maxResult, lockModeType);
            return list.stream().map(mapper).collect(ImmutableList.collector(list.size()));
        }

        @Override
        public boolean exist(int offset) {
            return collector.exist(offset);
        }

        @Override
        public <R1> Collector<R1> map(Function<? super R, ? extends R1> mapper) {
            return new MappedCollector<>(this.collector, this.mapper.andThen(mapper));
        }

        @Override
        public <X> SubQueryBuilder<X, R> asSubQuery() {
            throw new UnsupportedOperationException();
        }

    }
}
