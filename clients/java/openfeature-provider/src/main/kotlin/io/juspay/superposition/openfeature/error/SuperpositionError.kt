package io.juspay.superposition.openfeature.error

/**
 * Thrown when a Superposition provider operation fails.
 *
 * Extends [Exception], so it stays a *checked* exception: the `throws SuperpositionError` clauses
 * across the data-source and provider interfaces are the contract that forces callers to handle
 * failure, and Java code that implements or calls those interfaces still gets that from javac.
 *
 * @property errorCode The kind of failure this error represents.
 */
class SuperpositionError : Exception {

    val errorCode: ErrorCode

    /** The kinds of failure a provider can report. Shared with the Rust and Python clients. */
    enum class ErrorCode {
        /** Configuration or setup error. */
        CONFIG_ERROR,

        /** Network or HTTP error. */
        NETWORK_ERROR,

        /** Serialization or deserialization error. */
        SERIALIZATION_ERROR,

        /** The provider itself is unusable — typically evaluated before a successful init. */
        PROVIDER_ERROR,

        /** A data source failed. */
        DATA_SOURCE_ERROR,

        /** A refresh failed or outlived its timeout. */
        REFRESH_ERROR,
    }

    @JvmOverloads
    constructor(message: String, cause: Throwable? = null) : super(message, cause) {
        this.errorCode = ErrorCode.PROVIDER_ERROR
    }

    @JvmOverloads
    constructor(errorCode: ErrorCode, message: String, cause: Throwable? = null) :
        super(message, cause) {
        this.errorCode = errorCode
    }

    companion object {
        @JvmStatic
        @JvmOverloads
        fun configError(message: String, cause: Throwable? = null) =
            SuperpositionError(ErrorCode.CONFIG_ERROR, message, cause)

        @JvmStatic
        @JvmOverloads
        fun networkError(message: String, cause: Throwable? = null) =
            SuperpositionError(ErrorCode.NETWORK_ERROR, message, cause)

        @JvmStatic
        @JvmOverloads
        fun serializationError(message: String, cause: Throwable? = null) =
            SuperpositionError(ErrorCode.SERIALIZATION_ERROR, message, cause)

        @JvmStatic
        @JvmOverloads
        fun providerError(message: String, cause: Throwable? = null) =
            SuperpositionError(ErrorCode.PROVIDER_ERROR, message, cause)

        @JvmStatic
        @JvmOverloads
        fun dataSourceError(message: String, cause: Throwable? = null) =
            SuperpositionError(ErrorCode.DATA_SOURCE_ERROR, message, cause)

        @JvmStatic
        @JvmOverloads
        fun refreshError(message: String, cause: Throwable? = null) =
            SuperpositionError(ErrorCode.REFRESH_ERROR, message, cause)
    }
}
