package io.github.nextentity.core.util;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

/**
 * @author HuangChengwei
 * @since 2024/4/20 上午8:53
 */
public class Streams {

    public static <T, R> List<R> map(List<? extends T> list, Function<? super T, ? extends R> mapper) {
        List<R> result = new ArrayList<>();
        for (T t : list) {
            R r = mapper.apply(t);
            result.add(r);
        }
        return result;
    }

}
