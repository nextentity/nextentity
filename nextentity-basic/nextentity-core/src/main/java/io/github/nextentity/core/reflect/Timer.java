// package io.github.nextentity.core.reflect;
//
// import java.util.concurrent.atomic.AtomicLong;
//
// /**
//  * @author HuangChengwei
//  * @since 2024/4/20 下午3:11
//  */
// public class Timer {
//
//     AtomicLong time = new AtomicLong();
//     AtomicLong start = new AtomicLong();
//     AtomicLong count = new AtomicLong();
//
//     public void start() {
//         if (!start.compareAndSet(0, System.nanoTime())) {
//             throw new IllegalStateException("Timer already started");
//         }
//     }
//
//     public void stop() {
//         long start = this.start.getAndSet(0);
//         if (start == 0) {
//             throw new IllegalStateException("Timer already stopped");
//         }
//         time.addAndGet(System.nanoTime() - start);
//         count.incrementAndGet();
//     }
//
//     public AtomicLong time() {
//         return time;
//     }
//
//     public AtomicLong count() {
//         return count;
//     }
// }
