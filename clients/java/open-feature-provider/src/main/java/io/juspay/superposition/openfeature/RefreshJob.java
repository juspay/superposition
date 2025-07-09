package io.juspay.superposition.openfeature;

import io.juspay.superposition.openfeature.options.RefreshStrategy;
import lombok.extern.slf4j.Slf4j;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Optional;
import java.util.concurrent.*;
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
        private final CompletableFuture<T> output;
        private ScheduledFuture<?> poll;

        Poll(RefreshStrategy.Polling config, Supplier<CompletableFuture<T>> action) {
            this.config = config;
            this.action = action;
            this.output = new CompletableFuture<>();
        }

        void start() {
            log.debug("Starting polling-refresh.");
            poll = SEXEC.schedule(
                () -> {
                    var o = RefreshJob.runRefreshWithTimeout(action, config.timeout);
                    if (o != null) {
                        output.complete(o);
                    }
                },
                config.interval,
                TimeUnit.MILLISECONDS
            );
        }

        @Override
        public Optional<T> getOutput() {
            try {
                if (poll == null) {
                    log.warn("Polling hasn't started but the output is being used.");
                } else if (!poll.isCancelled() && !output.isDone()) {
                    return Optional.ofNullable(output.get(config.timeout, TimeUnit.MILLISECONDS));
                }
            } catch (Exception e) {
                log.warn("Attempted to await for poll output but an exception occurred: {}", e.toString());
            }
            return Optional.ofNullable(output.getNow(null));
        }

        @Override
        public void shutdown() {
            if (!poll.isCancelled()) {
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
        private boolean stopped = false;

        OnDemand(RefreshStrategy.OnDemand config, Supplier<CompletableFuture<T>> action) {
            this.config = config;
            this.action = action;
        }

        @Override
        public Optional<T> getOutput() {
            if (!stopped) {
                if (lastUpdated - System.currentTimeMillis() < config.ttl) {
                    log.debug("Running refresh as current output is stale.");
                    var o = RefreshJob.runRefreshWithTimeout(action, config.timeout);
                    if (o != null) {
                        output = o;
                        lastUpdated = System.currentTimeMillis();
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
        if (config instanceof RefreshStrategy.Polling) {
            return new Poll<>((RefreshStrategy.Polling)config, action);
        } else if (config instanceof RefreshStrategy.OnDemand) {
            return new OnDemand<>((RefreshStrategy.OnDemand)config, action);
        }
        throw new IllegalArgumentException("Invalid refresh-strategy: " + config);
    }
}
