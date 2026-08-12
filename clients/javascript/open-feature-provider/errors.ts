/**
 * Typed errors raised by the Superposition provider.
 *
 * A single error type with a `code` discriminant, mirroring the Rust/Python/Java `SuperpositionError`
 * so a failure means the same thing in every client. Consumers can `catch` it off the data-source and
 * lifecycle surface (initialize / refresh / close) and branch on `code`. The normal OpenFeature
 * flag-evaluation path flattens any throw into an OpenFeature GENERAL error, so `code` matters most
 * when the provider or its data sources are used directly.
 *
 * (No `SERIALIZATION_ERROR` variant, unlike the other clients: the JS binding hands back already-parsed
 * values, so there is no per-key decode step that could fail — advertising a code that cannot occur
 * would be a false affordance.)
 */

/** The category of failure a {@link SuperpositionError} represents. */
export enum ErrorCode {
    CONFIG_ERROR = "CONFIG_ERROR",
    NETWORK_ERROR = "NETWORK_ERROR",
    PROVIDER_ERROR = "PROVIDER_ERROR",
    DATA_SOURCE_ERROR = "DATA_SOURCE_ERROR",
    REFRESH_ERROR = "REFRESH_ERROR",
}

/** Raised when a provider or data source operation fails. */
export class SuperpositionError extends Error {
    readonly code: ErrorCode;

    constructor(code: ErrorCode, message: string, cause?: unknown) {
        super(message, cause !== undefined ? { cause } : undefined);
        this.name = "SuperpositionError";
        this.code = code;
    }

    override toString(): string {
        return `${this.name} [${this.code}]: ${this.message}`;
    }

    // --- Factories, one per variant ---

    static configError(message: string, cause?: unknown): SuperpositionError {
        return new SuperpositionError(ErrorCode.CONFIG_ERROR, message, cause);
    }

    static networkError(message: string, cause?: unknown): SuperpositionError {
        return new SuperpositionError(ErrorCode.NETWORK_ERROR, message, cause);
    }

    static providerError(message: string, cause?: unknown): SuperpositionError {
        return new SuperpositionError(ErrorCode.PROVIDER_ERROR, message, cause);
    }

    static dataSourceError(
        message: string,
        cause?: unknown,
    ): SuperpositionError {
        return new SuperpositionError(
            ErrorCode.DATA_SOURCE_ERROR,
            message,
            cause,
        );
    }

    static refreshError(message: string, cause?: unknown): SuperpositionError {
        return new SuperpositionError(ErrorCode.REFRESH_ERROR, message, cause);
    }
}
