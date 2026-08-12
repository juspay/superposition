/**
 * Data source abstraction for fetching configuration and experiment data.
 *
 * A unified interface over different transport mechanisms (HTTP, file-based) for fetching
 * configuration and experiment data from a Superposition backend. Mirrors the Rust/Python/Java
 * `SuperpositionDataSource` + `FetchResponse`.
 */

import type { Config, ExperimentConfig } from "superposition-bindings";

// The native FFI shapes live in the bindings package (like Python/Java's UniFFI-generated types).
// `Config` is the raw config the native cache's initConfig consumes and `ExperimentConfig` is the
// experiments in its cache shape. Re-exported so consumers get them alongside the wrapper types below.
export type { Config, ExperimentConfig };

/** Configuration data with fetch metadata. */
export interface ConfigData {
    data: Config;
    /** When the source reports the config was last modified (HTTP) or last read (file). */
    fetchedAt: Date;
}

/** Experiment data with fetch metadata. */
export interface ExperimentData {
    data: ExperimentConfig;
    fetchedAt: Date;
}

/**
 * Either fetched data or a 304 Not Modified marker.
 *
 * A true sum type: `NotModified` is a distinct variant rather than "data is null", so a source that
 * legitimately returns nothing cannot be mistaken for an unchanged one.
 */
export class FetchResponse<T> {
    private constructor(
        private readonly _data: T | null,
        private readonly _notModified: boolean
    ) {}

    /** A successful response carrying data. */
    static data<T>(data: T): FetchResponse<T> {
        return new FetchResponse<T>(data, false);
    }

    /** A 304 Not Modified response. */
    static notModified<T>(): FetchResponse<T> {
        return new FetchResponse<T>(null, true);
    }

    isNotModified(): boolean {
        return this._notModified;
    }

    /** The response data, or `null` if not modified. */
    getData(): T | null {
        return this._data;
    }

    /** Transform the data if present, preserving a NotModified response. */
    mapData<U>(mapper: (data: T) => U): FetchResponse<U> {
        if (this._notModified) {
            return FetchResponse.notModified<U>();
        }
        return FetchResponse.data<U>(mapper(this._data as T));
    }
}

/**
 * Fetches configuration and experiment data. Implementors provide the transport mechanism (HTTP,
 * file-based, etc.); consumers interact with this unified interface.
 *
 * `fetchConfig` has a default (delegates to `fetchFilteredConfig`); everything else is required.
 */
export interface SuperpositionDataSource {
    /** Fetch the full resolved configuration. */
    fetchConfig(ifModifiedSince?: Date): Promise<FetchResponse<ConfigData>>;

    /** Fetch resolved configuration filtered by context and prefixes. */
    fetchFilteredConfig(
        context?: Record<string, any>,
        prefixFilter?: string[],
        excludePrefixFilter?: string[],
        ifModifiedSince?: Date
    ): Promise<FetchResponse<ConfigData>>;

    /** Fetch all active experiments. */
    fetchActiveExperiments(ifModifiedSince?: Date): Promise<FetchResponse<ExperimentData>>;

    /** Fetch active experiments with conditions matching the context (candidate). */
    fetchCandidateActiveExperiments(
        context?: Record<string, any>,
        prefixFilter?: string[],
        excludePrefixFilter?: string[],
        ifModifiedSince?: Date
    ): Promise<FetchResponse<ExperimentData>>;

    /** Fetch active experiments that match the context. */
    fetchMatchingActiveExperiments(
        context?: Record<string, any>,
        prefixFilter?: string[],
        excludePrefixFilter?: string[],
        ifModifiedSince?: Date
    ): Promise<FetchResponse<ExperimentData>>;

    /** Whether this data source supports experiments. */
    supportsExperiments(): boolean;

    /**
     * Watch the underlying source for changes, yielding on every change. Returns `null` if the
     * source does not support watching.
     */
    watch(): AsyncGenerator<string, void, unknown> | null;

    /** Clean up any resources held by this data source. */
    close(): Promise<void>;
}

/**
 * Base class supplying the `fetchConfig` default so concrete sources need only implement
 * `fetchFilteredConfig`. Concrete sources may `extend` this, or implement the interface directly.
 */
export abstract class BaseDataSource implements SuperpositionDataSource {
    fetchConfig(ifModifiedSince?: Date): Promise<FetchResponse<ConfigData>> {
        return this.fetchFilteredConfig(undefined, undefined, undefined, ifModifiedSince);
    }

    abstract fetchFilteredConfig(
        context?: Record<string, any>,
        prefixFilter?: string[],
        excludePrefixFilter?: string[],
        ifModifiedSince?: Date
    ): Promise<FetchResponse<ConfigData>>;

    abstract fetchActiveExperiments(
        ifModifiedSince?: Date
    ): Promise<FetchResponse<ExperimentData>>;

    abstract fetchCandidateActiveExperiments(
        context?: Record<string, any>,
        prefixFilter?: string[],
        excludePrefixFilter?: string[],
        ifModifiedSince?: Date
    ): Promise<FetchResponse<ExperimentData>>;

    abstract fetchMatchingActiveExperiments(
        context?: Record<string, any>,
        prefixFilter?: string[],
        excludePrefixFilter?: string[],
        ifModifiedSince?: Date
    ): Promise<FetchResponse<ExperimentData>>;

    supportsExperiments(): boolean {
        return false;
    }

    watch(): AsyncGenerator<string, void, unknown> | null {
        return null;
    }

    abstract close(): Promise<void>;
}
