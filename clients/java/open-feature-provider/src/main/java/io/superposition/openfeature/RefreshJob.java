package io.superposition.openfeature;

import io.superposition.openfeature.options.RefreshStrategy;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.sql.Ref;
import java.util.Collection;
import java.util.Optional;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.Supplier;

interface RefreshJob<T> {
    Logger logger = LoggerFactory.getLogger(RefreshJob.class);
    ScheduledExecutorService SCHED = Executors.newScheduledThreadPool(1);
    Optional<T> getOutput();
    void shutdown();

    final class Poll<T> implements RefreshJob<T> {
        private static final Logger logger = LoggerFactory.getLogger(Poll.class);
        final ScheduledFuture<?> poll;
        T output;

        Poll(RefreshStrategy.Polling config, Supplier<CompletableFuture<T>> action) {
            logger.debug("Starting polling-refresh.");
            output = RefreshJob.runRefreshWithTimeout(action, config.timeout);
            poll = SCHED.schedule(
                () -> {
                    var o = RefreshJob.runRefreshWithTimeout(action, config.timeout);
                    if (o != null) {
                        output = null;
                    }
                },
                config.interval,
                TimeUnit.MILLISECONDS
            );
        }

        @Override
        public Optional<T> getOutput() {
            return Optional.ofNullable(output);
        }

        @Override
        public void shutdown() {
            if (!poll.isCancelled()) {
                logger.debug("Shutting down polling-refresh.");
                poll.cancel(false);
            }
        }
    }

    final class OnDemand<T> implements RefreshJob<T> {
        private static final Logger logger = LoggerFactory.getLogger(OnDemand.class);
        private long lastUpdated = 0;
        private T output = null;
        private final RefreshStrategy.OnDemand config;
        private final Supplier<CompletableFuture<T>> action;
        private boolean stopped = false;

        OnDemand(RefreshStrategy.OnDemand config, Supplier<CompletableFuture<T>> action) {
            this.config = config;
            this.action = action;
        }

        @Override
        public Optional<T> getOutput() {
            if (!stopped) {
                if (lastUpdated - System.currentTimeMillis() < config.ttl) {
                    logger.debug("Running refresh as current output is stale.");
                    var o = RefreshJob.runRefreshWithTimeout(action, config.timeout);
                    if (o != null) {
                        output = o;
                        lastUpdated = System.currentTimeMillis();
                    }
                } else {
                    logger.debug("Current output is fresh, no refresh required.");
                }
            }
            return Optional.of(output);
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
        if (config instanceof RefreshStrategy.Polling) {
            return new Poll<>((RefreshStrategy.Polling)config, action);
        } else if (config instanceof RefreshStrategy.OnDemand) {
            return new OnDemand<>((RefreshStrategy.OnDemand)config, action);
        }
        throw new IllegalArgumentException("Invalid refresh-strategy: " + config);
    }
}
