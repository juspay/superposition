package io.juspay.superposition.openfeature.data_source

import java.util.concurrent.BlockingQueue
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.TimeUnit

/**
 * A stream of change notifications from a data source.
 *
 * Used for watching file system changes or other event-based data source updates. Consumers poll
 * or block on [getNextEvent] to receive notifications.
 *
 * Each subscriber gets its own stream: the data source fans every change out to all live streams,
 * so one consumer cannot steal another's events.
 */
class WatchStream {

    private val eventQueue: BlockingQueue<Boolean> = LinkedBlockingQueue()

    /** Whether this stream has been closed by its data source. */
    @Volatile
    var isClosed: Boolean = false
        private set

    /** Notify that a change has occurred. Safe to call from the watching thread. */
    fun notifyChange() {
        if (!isClosed) {
            eventQueue.offer(true)
        }
    }

    /**
     * Close the stream, releasing any consumer blocked in [getNextEvent].
     *
     * Subsequent and in-flight waits return false.
     */
    fun close() {
        isClosed = true
        eventQueue.offer(CLOSED)
    }

    /**
     * Wait for the next event, blocking until one arrives.
     *
     * @return true if a change occurred, false if the stream was closed
     */
    @Throws(InterruptedException::class)
    fun getNextEvent(): Boolean {
        // Check before blocking: close() enqueues only one CLOSED sentinel, so if it was already
        // consumed (e.g. by the debounce drain), take() would block forever with nothing to wake it.
        if (isClosed) return false
        return eventQueue.take() && !isClosed
    }

    /**
     * Wait for the next event, giving up after [timeoutMs].
     *
     * @return true if a change occurred, false on timeout or if the stream is closed
     */
    @Throws(InterruptedException::class)
    fun tryGetNextEvent(timeoutMs: Long): Boolean {
        // See getNextEvent: without this a closed stream whose sentinel was already drained costs a
        // full timeoutMs before the caller learns it is closed.
        if (isClosed) return false
        return eventQueue.poll(timeoutMs, TimeUnit.MILLISECONDS) == true && !isClosed
    }

    /** Whether any events are queued, without blocking. */
    fun hasPendingEvents(): Boolean = eventQueue.isNotEmpty()

    /** Discard all queued events. */
    fun drainEvents(): Int {
        var count = 0
        while (eventQueue.poll() != null) {
            count++
        }
        return count
    }

    private companion object {
        /** Enqueued by [close] to release consumers blocked in [getNextEvent]. */
        const val CLOSED = false
    }
}
