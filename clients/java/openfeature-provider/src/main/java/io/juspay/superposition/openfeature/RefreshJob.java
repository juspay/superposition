package io.juspay.superposition.openfeature;

import io.juspay.superposition.openfeature.options.RefreshStrategy;
import lombok.extern.slf4j.Slf4j;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.lang.ref.WeakReference;
import java.util.Optional;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Supplier;

interface RefreshJob<T> {
    Logger logger = LoggerFactory.getLogger(RefreshJob.class);
    ScheduledExecutorService SEXEC = Executors.newScheduledThreadPool(2);
    Optional<T> getOutput();
    void shutdown();

    @Slf4j
    final class Poll<T> implements RefreshJob<T> {
        private final RefreshStrategy.Polling config;
        private final Supplier<CompletableFuture<T>> action;
        private final Runnable onChange;
        private final CompletableFuture<T> firstOutput;
        private volatile T latestOutput = null;
        private ScheduledFuture<?> poll;

        Poll(RefreshStrategy.Polling config, Supplier<CompletableFuture<T>> action, Runnable onChange) {
            this.config = config;
            this.action = action;
            this.onChange = onChange;
            this.firstOutput = new CompletableFuture<>();
        }

        void start() {
            log.debug("Starting polling-refresh.");
            WeakReference<Poll<T>> weakSelf = new WeakReference<>(this);
            AtomicReference<ScheduledFuture<?>> taskRef = new AtomicReference<>();

            ScheduledFuture<?> scheduled = SEXEC.scheduleAtFixedRate(
                () -> {
                    Poll<T> self = weakSelf.get();
                    if (self == null) {
                        log.debug("Poll referent GC'd — self-cancelling polling task.");
                        ScheduledFuture<?> t = taskRef.get();
                        if (t != null) t.cancel(false);
                        return;
                    }
                    var o = RefreshJob.runRefreshWithTimeout(self.action, self.config.timeout);
                    if (o != null) {
                        boolean changed = !o.equals(self.latestOutput);
                        self.latestOutput = o;
                        if (!self.firstOutput.isDone()) {
                            self.firstOutput.complete(o);
                        }
                        if (changed && self.onChange != null) {
                            try {
                                self.onChange.run();
                            } catch (Exception e) {
                                log.error("onChange callback error: {}", e.getMessage());
                            }
                        } else if (!changed) {
                            log.debug("Output unchanged, skipping onChange callback.");
                        }
                    }
                },
                0,
                config.interval,
                TimeUnit.MILLISECONDS
            );

            taskRef.set(scheduled);
            this.poll = scheduled;
        }

        @Override
        public Optional<T> getOutput() {
            if (latestOutput != null) {
                return Optional.of(latestOutput);
            }
            try {
                if (poll == null) {
                    log.warn("Polling hasn't started but the output is being used.");
                } else if (!firstOutput.isDone()) {
                    return Optional.ofNullable(firstOutput.get(config.timeout, TimeUnit.MILLISECONDS));
                }
            } catch (Exception e) {
                log.warn("Attempted to await for poll output but an exception occurred: {}", e.toString());
            }
            return Optional.ofNullable(latestOutput);
        }

        @Override
        public void shutdown() {
            if (poll != null && !poll.isCancelled()) {
                log.debug("Shutting down polling-refresh.");
                poll.cancel(false);
            }
        }
    }

    @Slf4j
    final class OnDemand<T> implements RefreshJob<T> {
        private long lastUpdated = 0;
        private T output = null;
        private final RefreshStrategy.OnDemand config;
        private final Supplier<CompletableFuture<T>> action;
        private final Runnable onChange;
        private boolean stopped = false;

        OnDemand(RefreshStrategy.OnDemand config, Supplier<CompletableFuture<T>> action, Runnable onChange) {
            this.config = config;
            this.action = action;
            this.onChange = onChange;
        }

        @Override
        public Optional<T> getOutput() {
            if (!stopped) {
                if (lastUpdated - System.currentTimeMillis() < config.ttl) {
                    log.debug("Running refresh as current output is stale.");
                    var o = RefreshJob.runRefreshWithTimeout(action, config.timeout);
                    if (o != null) {
                        boolean changed = !o.equals(output);
                        output = o;
                        lastUpdated = System.currentTimeMillis();
                        if (changed && onChange != null) {
                            try {
                                onChange.run();
                            } catch (Exception e) {
                                log.error("onChange callback error: {}", e.getMessage());
                            }
                        } else if (!changed) {
                            log.debug("Output unchanged, skipping onChange callback.");
                        }
                    }
                } else {
                    log.debug("Current output is fresh, no refresh required.");
                }
            }
            return Optional.ofNullable(output);
        }

        @Override
        public void shutdown() {
            stopped = true;
        }
    }

    private static<T> T runRefreshWithTimeout(Supplier<CompletableFuture<T>> action, int timeout) {
        try {
            return action.get().get(timeout, TimeUnit.MILLISECONDS);
        } catch (TimeoutException e) {
           logger.error("Refresh action timed-out.");
           return null;
        } catch (Exception e) {
            logger.error("An exception occurred while running a refresh action.\nMessage: {}.\nStack-Trace: {}", e.getMessage(), e.getStackTrace());
            return null;
        }
    }

    static <T> RefreshJob<T> create(RefreshStrategy config, Supplier<CompletableFuture<T>> action) {
        return create(config, action, null);
    }

    static <T> RefreshJob<T> create(RefreshStrategy config, Supplier<CompletableFuture<T>> action, Runnable onChange) {
        if (config instanceof RefreshStrategy.Polling) {
            return new Poll<>((RefreshStrategy.Polling)config, action, onChange);
        } else if (config instanceof RefreshStrategy.OnDemand) {
            return new OnDemand<>((RefreshStrategy.OnDemand)config, action, onChange);
        }
        throw new IllegalArgumentException("Invalid refresh-strategy: " + config);
    }
}
