package io.juspay.superposition.openfeature.data_source

import java.util.Optional
import java.util.function.Function

/**
 * The result of a fetch: either fresh data, or confirmation that nothing changed.
 *
 * A data source that supports conditional fetching (HTTP 304 / If-Modified-Since) answers
 * [NotModified] when the caller's cached copy is still current, which is not an error and not an
 * empty result — it means "keep what you have".
 *
 * @param T the type of data returned by the fetch operation
 */
sealed class FetchResponse<out T : Any> {

    /**
     * The data has not been modified since the last request. Corresponds to HTTP 304.
     *
     * A single object serves every `T`: it carries no value, so `FetchResponse<Nothing>` is a
     * subtype of `FetchResponse<T>` for any `T`. The Java version had to share one instance
     * behind an unchecked cast to achieve the same thing.
     */
    object NotModified : FetchResponse<Nothing>() {
        override fun toString(): String = "FetchResponse.NotModified"
    }

    /** Fresh data from the source. */
    data class Data<T : Any>(val value: T) : FetchResponse<T>() {
        override fun toString(): String = "FetchResponse.Data($value)"
    }

    /** Whether this response means "nothing changed". */
    fun isNotModified(): Boolean = this is NotModified

    /** The data, or empty when nothing changed. */
    fun getData(): Optional<@UnsafeVariance T> = when (this) {
        is NotModified -> Optional.empty()
        is Data -> Optional.of(value)
    }

    /**
     * Transform the data if there is any, preserving [NotModified].
     *
     * `T` only ever flows out of the mapper's input here, so the variance escape is sound.
     */
    fun <U : Any> mapData(mapper: Function<@UnsafeVariance T, U>): FetchResponse<U> = when (this) {
        is NotModified -> NotModified
        is Data -> Data(mapper.apply(value))
    }

    companion object {
        /** A response meaning "your cached copy is still current". */
        @JvmStatic
        fun <T : Any> notModified(): FetchResponse<T> = NotModified

        /** A response carrying fresh data. */
        @JvmStatic
        fun <T : Any> data(data: T): FetchResponse<T> = Data(data)
    }
}
