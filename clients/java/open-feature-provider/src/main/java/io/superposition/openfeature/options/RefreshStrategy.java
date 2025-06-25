package io.superposition.openfeature.options;

// TODO Doc
public interface RefreshStrategy {
    public int getTimeout();
    // TODO Doc
    public static final class Polling implements RefreshStrategy {
        public int timeout;
        public int interval;

        Polling(int timeout, int interval) {
            // Set -1 for no timeout.
            this.timeout = timeout;
            this.interval = interval;
        }

        public static RefreshStrategy of(int timeout, int interval) {
            return new Polling(timeout, interval);
        }

        @Override
        public int getTimeout() {
            return timeout;
        }
    }

    // TODO Doc
    public static final class OnDemand implements RefreshStrategy {
        // Set -1 for no timeout.
        public int timeout;
        public int ttl;

        OnDemand(int timeout, int interval) {
            this.timeout = timeout;
            this.ttl = interval;
        }

        @Override
        public int getTimeout() {
            return timeout;
        }

        public static RefreshStrategy of(int timeout, int ttl) {
            return new OnDemand(timeout, ttl);
        }
    }
}
