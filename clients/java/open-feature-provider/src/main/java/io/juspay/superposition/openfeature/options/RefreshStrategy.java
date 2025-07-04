package io.juspay.superposition.openfeature.options;

/**
 * Strategy interface for controlling how feature flag data is refreshed from the backend.
 * Implementations define the refresh mechanism and timeout behavior.
 */
public interface RefreshStrategy {
    /**
     * Returns the timeout for the refresh operation, in milliseconds.
     */
    int getTimeout();

    /**
     * Refresh strategy that polls the backend at a fixed interval.
     * Useful for keeping data up-to-date in near real-time.
     */
    final class Polling implements RefreshStrategy {
        /** Timeout for polling, in milliseconds. */
        public int timeout;
        /** Polling interval, in milliseconds. */
        public int interval;

        /**
         * Creates a polling refresh strategy.
         * @param timeout Timeout in milliseconds
         * @param interval Polling interval in milliseconds
         */
        Polling(int timeout, int interval) {
            this.timeout = timeout;
            this.interval = interval;
        }

        /**
         * Factory method for creating a polling refresh strategy.
         * @param timeout Timeout in milliseconds
         * @param interval Polling interval in milliseconds
         * @return a new Polling RefreshStrategy
         */
        public static RefreshStrategy of(int timeout, int interval) {
            return new Polling(timeout, interval);
        }

        @Override
        public int getTimeout() {
            return timeout;
        }
    }

    /**
     * Refresh strategy that fetches data on demand and caches it for a TTL (time-to-live).
     * Useful for reducing backend calls and controlling staleness.
     */
    final class OnDemand implements RefreshStrategy {
        /** Timeout for on-demand fetch, in milliseconds. */
        public int timeout;
        /** Time-to-live (TTL) for cached data, in milliseconds. */
        public int ttl;

        /**
         * Creates an on-demand refresh strategy.
         * @param timeout Timeout in milliseconds
         * @param ttl Time-to-live for cached data in milliseconds
         */
        OnDemand(int timeout, int ttl) {
            this.timeout = timeout;
            this.ttl = ttl;
        }

        @Override
        public int getTimeout() {
            return timeout;
        }

        /**
         * Factory method for creating an on-demand refresh strategy.
         * @param timeout Timeout in milliseconds
         * @param ttl Time-to-live for cached data in milliseconds
         * @return a new OnDemand RefreshStrategy
         */
        public static RefreshStrategy of(int timeout, int ttl) {
            return new OnDemand(timeout, ttl);
        }
    }
}
