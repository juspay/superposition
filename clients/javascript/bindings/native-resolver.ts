import path from "path";
import os from "os";
import fs from "fs";
import koffi from "koffi";
import { fileURLToPath } from "url";
import { Buffer } from "buffer";

// Node < 20 lacks the well-known `Symbol.dispose`; define it via the global registry (so every
// polyfill agrees on the same symbol) so `using` works there. No-op on Node >= 20, which has it.
{
    const sym = Symbol as unknown as { dispose?: symbol };
    if (typeof sym.dispose !== "symbol") {
        sym.dispose = Symbol.for("Symbol.dispose");
    }
}

const ERROR_BUFFER_SIZE = 2048;

/**
 * Native FFI shapes consumed and produced by the resolver and provider cache.
 *
 * These are the JS analog of the types the Python and Java bindings expose (UniFFI-generated
 * `Config`/`ExperimentConfig`/`ProviderCache`) and of Rust's `superposition_types::Config`. The JS
 * bindings are hand-written rather than code-generated, so the shapes are declared by hand — but they
 * live here in the bindings package, so consumers import them from one place instead of redeclaring
 * them.
 */

/**
 * A context: a condition matched against an evaluation context, plus the override keys it selects.
 * Scalar fields are typed; the nested `condition` is a loose map — the fully-typed `Condition` /
 * `DimensionType` / `DependencyGraph` shapes the UniFFI-generated Python/Java bindings model aren't
 * transcribed here (hand-maintained, they'd be drift-prone against the Rust types).
 */
export interface Context {
    id: string;
    /** Dimension name → matched value/criteria (a serialized `Condition`). */
    condition: Record<string, any>;
    priority: number;
    weight: number;
    override_with_keys: string[];
}

/**
 * A dimension's kind. Serialized externally-tagged (matching the Rust enum) — always a single-key
 * object, never a bare string:
 * - `{ REGULAR: {} }`
 * - `{ LOCAL_COHORT: "<cohort>" }`
 * - `{ REMOTE_COHORT: "<cohort>" }`
 */
export type DimensionType =
    | { REGULAR: Record<string, never> }
    | { LOCAL_COHORT: string }
    | { REMOTE_COHORT: string };

/** Metadata describing a single dimension. Nested `schema`/`dependency_graph` are loose (see {@link Context}). */
export interface DimensionInfo {
    schema: Record<string, any>;
    position: number;
    dimension_type: DimensionType;
    dependency_graph: Record<string, any>;
    value_compute_function_name?: string;
    description?: string;
}

/** Resolved configuration in the shape the cache's {@link ProviderCache.initConfig} consumes. */
export interface Config {
    default_configs: Record<string, any>;
    contexts: Context[];
    overrides: Record<string, Record<string, any>>;
    dimensions: Record<string, DimensionInfo>;
}

/** A single experiment variant. */
export interface Variant {
    id: string;
    variant_type: "CONTROL" | "EXPERIMENTAL";
    context_id?: string;
    override_id?: string;
    overrides: Record<string, string>;
}

/** An experiment in the shape the native cache consumes. */
export interface FfiExperiment {
    id: string;
    context: Record<string, string>;
    variants: Variant[];
    traffic_percentage: number;
    // The native cache deserializes experiments into a struct that carries the status, so it must be
    // present; the values mirror the SDK's ExperimentStatusType.
    status: "CREATED" | "INPROGRESS" | "CONCLUDED" | "PAUSED" | "DISCARDED";
}

/** A variant/experiment mapping within an experiment group. */
export interface Bucket {
    variant_id: string;
    experiment_id: string;
}

/** An experiment group in the shape the native cache consumes. */
export interface FfiExperimentGroup {
    id: string;
    context: Record<string, string>;
    traffic_percentage: number;
    member_experiment_ids: string[];
    group_type: "SYSTEM_GENERATED" | "USER_CREATED";
    buckets: Bucket[];
}

/** Experiments in the shape the cache's {@link ProviderCache.initExperiments} consumes. */
export interface ExperimentConfig {
    experiments: FfiExperiment[];
    experiment_groups: FfiExperimentGroup[];
}

/**
 * A handle to the native provider cache. Mirrors the UniFFI-generated `ProviderCache` class in the
 * Python/Java bindings (Java's is `AutoCloseable`). Create one via
 * {@link NativeResolver.createProviderCache} — do not construct it directly.
 *
 * It owns a native pointer, so it must be released: call {@link free} (or {@link close}) when done, or
 * bind it with a `using` declaration to release it at scope exit via {@link Symbol.dispose}. Release is
 * idempotent, and any use after release throws rather than touching a freed pointer.
 */
export class ProviderCache {
    private readonly handle: unknown;
    private freed = false;

    /** @internal Prefer {@link NativeResolver.createProviderCache}. */
    constructor(private readonly lib: any) {
        this.handle = lib.core_provider_cache_new();
        if (!this.handle) {
            throw new Error("core_provider_cache_new returned null");
        }
    }

    private assertLive(): void {
        if (this.freed) {
            throw new Error(
                "ProviderCache has been freed and can no longer be used",
            );
        }
    }

    /** Load resolved configuration into the cache (replaces any previous config). */
    initConfig(
        defaultConfigs: Record<string, any>,
        contexts: any[],
        overrides: Record<string, any>,
        dimensions: Record<string, any>,
    ): void {
        this.assertLive();
        const ebuf = Buffer.alloc(ERROR_BUFFER_SIZE);
        this.lib.core_provider_cache_init_config(
            this.handle,
            JSON.stringify(defaultConfigs || {}),
            JSON.stringify(contexts || []),
            JSON.stringify(overrides || {}),
            JSON.stringify(dimensions || {}),
            ebuf,
        );
        const err = ebuf.toString("utf8").split("\0")[0];
        if (err.length !== 0) throw new Error("ffi: " + err);
    }

    /** Load experiments into the cache (replaces any previous experiments). */
    initExperiments(
        experiments: FfiExperiment[],
        experimentGroups: FfiExperimentGroup[],
    ): void {
        this.assertLive();
        const ebuf = Buffer.alloc(ERROR_BUFFER_SIZE);
        this.lib.core_provider_cache_init_experiments(
            this.handle,
            JSON.stringify(experiments || []),
            JSON.stringify(experimentGroups || []),
            ebuf,
        );
        const err = ebuf.toString("utf8").split("\0")[0];
        if (err.length !== 0) throw new Error("ffi: " + err);
    }

    /** Evaluate the cached config against `queryData`, returning real typed values keyed by flag. */
    evalConfig(
        queryData: Record<string, any>,
        mergeStrategy: string = "merge",
        filterPrefixes?: string[],
        filterExcludePrefixes?: string[],
        targetingKey?: string | null,
    ): Record<string, any> {
        this.assertLive();
        const ebuf = Buffer.alloc(ERROR_BUFFER_SIZE);
        const filterPrefixesJson =
            filterPrefixes && filterPrefixes.length > 0
                ? JSON.stringify(filterPrefixes)
                : null;
        const filterExcludePrefixesJson =
            filterExcludePrefixes && filterExcludePrefixes.length > 0
                ? JSON.stringify(filterExcludePrefixes)
                : null;
        const result = this.lib.core_provider_cache_eval_config(
            this.handle,
            JSON.stringify(queryData || {}),
            mergeStrategy,
            filterPrefixesJson,
            filterExcludePrefixesJson,
            targetingKey || null,
            ebuf,
        );
        const err = ebuf.toString("utf8").split("\0")[0];
        if (err.length !== 0) throw new Error("ffi: " + err);
        const configStr =
            typeof result === "string"
                ? result
                : this.lib.decode(result, "string");
        if (typeof result !== "string") this.lib.core_free_string(result);
        return JSON.parse(configStr);
    }

    /**
     * Filter the cached config by dimension data / prefixes, returning the filtered {@link Config}.
     * Mirrors the UniFFI `ProviderCache.filter_config` used by the Python/Java data sources.
     */
    filterConfig(
        dimensionData?: Record<string, any>,
        filterPrefixes?: string[],
        filterExcludePrefixes?: string[],
    ): Config {
        this.assertLive();
        const ebuf = Buffer.alloc(ERROR_BUFFER_SIZE);
        const dimensionDataJson =
            dimensionData && Object.keys(dimensionData).length > 0
                ? JSON.stringify(dimensionData)
                : null;
        const filterPrefixesJson =
            filterPrefixes && filterPrefixes.length > 0
                ? JSON.stringify(filterPrefixes)
                : null;
        const filterExcludePrefixesJson =
            filterExcludePrefixes && filterExcludePrefixes.length > 0
                ? JSON.stringify(filterExcludePrefixes)
                : null;
        const result = this.lib.core_provider_cache_filter_config(
            this.handle,
            dimensionDataJson,
            filterPrefixesJson,
            filterExcludePrefixesJson,
            ebuf,
        );
        return this.decodeResult(result, ebuf);
    }

    /**
     * Filter the cached experiments by dimension data / prefixes, returning the filtered
     * {@link ExperimentConfig}. `partialApply` selects candidate (`true`) vs matching-active (`false`)
     * semantics. Mirrors the UniFFI `ProviderCache.filter_experiment`.
     */
    filterExperiment(
        dimensionData?: Record<string, any>,
        filterPrefixes?: string[],
        filterExcludePrefixes?: string[],
        partialApply: boolean = false,
    ): ExperimentConfig {
        this.assertLive();
        const ebuf = Buffer.alloc(ERROR_BUFFER_SIZE);
        const dimensionDataJson =
            dimensionData && Object.keys(dimensionData).length > 0
                ? JSON.stringify(dimensionData)
                : null;
        const filterPrefixesJson =
            filterPrefixes && filterPrefixes.length > 0
                ? JSON.stringify(filterPrefixes)
                : null;
        const filterExcludePrefixesJson =
            filterExcludePrefixes && filterExcludePrefixes.length > 0
                ? JSON.stringify(filterExcludePrefixes)
                : null;
        const result = this.lib.core_provider_cache_filter_experiment(
            this.handle,
            dimensionDataJson,
            filterPrefixesJson,
            filterExcludePrefixesJson,
            partialApply,
            ebuf,
        );
        return this.decodeResult(result, ebuf);
    }

    /**
     * Get the applicable experiment variant IDs from the cached experiments for `targetingKey`.
     * Mirrors the UniFFI `ProviderCache.get_applicable_variants`.
     */
    getApplicableVariants(
        dimensionData?: Record<string, any>,
        filterPrefixes?: string[],
        filterExcludePrefixes?: string[],
        targetingKey?: string | null,
    ): string[] {
        this.assertLive();
        const ebuf = Buffer.alloc(ERROR_BUFFER_SIZE);
        const dimensionDataJson =
            dimensionData && Object.keys(dimensionData).length > 0
                ? JSON.stringify(dimensionData)
                : null;
        const filterPrefixesJson =
            filterPrefixes && filterPrefixes.length > 0
                ? JSON.stringify(filterPrefixes)
                : null;
        const filterExcludePrefixesJson =
            filterExcludePrefixes && filterExcludePrefixes.length > 0
                ? JSON.stringify(filterExcludePrefixes)
                : null;
        const result = this.lib.core_provider_cache_get_applicable_variants(
            this.handle,
            dimensionDataJson,
            filterPrefixesJson,
            filterExcludePrefixesJson,
            targetingKey || null,
            ebuf,
        );
        return this.decodeResult(result, ebuf);
    }

    /**
     * Decode a `char*` result from an FFI call: raise on a non-empty error buffer, decode + free the
     * native string, and JSON-parse it.
     */
    private decodeResult(result: unknown, ebuf: Buffer): any {
        const err = ebuf.toString("utf8").split("\0")[0];
        if (err.length !== 0) throw new Error("ffi: " + err);
        const str =
            typeof result === "string"
                ? result
                : this.lib.decode(result, "string");
        if (typeof result !== "string") this.lib.core_free_string(result);
        return JSON.parse(str);
    }

    /** Release the native handle. Idempotent; using the cache afterwards throws. */
    free(): void {
        if (this.freed) return;
        this.freed = true;
        this.lib.core_provider_cache_free(this.handle);
    }

    /** Alias for {@link free} (AutoCloseable-style). */
    close(): void {
        this.free();
    }

    /** Releases the handle when the cache is bound with a `using` declaration. */
    [Symbol.dispose](): void {
        this.free();
    }
}

export class NativeResolver {
    private lib: any;
    private isAvailable: boolean = false;

    constructor(libPath?: string) {
        try {
            this.lib = koffi.load(libPath || this.getDefaultLibPath());

            // Define the core resolution functions with CORRECT 8 parameters each
            this.lib.core_get_resolved_config = this.lib.func(
                "char* core_get_resolved_config(const char*, const char*, const char*, const char*, const char*, const char*, const char*, const char*, const char*, const char*)",
            );
            this.lib.core_free_string = this.lib.func(
                "void core_free_string(char*)",
            );
            this.lib.core_get_applicable_variants = this.lib.func(
                "char* core_get_applicable_variants(const char*, const char*, const char*, const char*, const char*, const char*, const char*, char*)",
            );
            this.lib.core_test_connection = this.lib.func(
                "int core_test_connection()",
            );
            this.lib.core_parse_config_file_with_filters = this.lib.func(
                "char* core_parse_config_file_with_filters(const char*, const char*, const char*, const char*, const char*, char*)",
            );
            this.lib.core_provider_cache_new = this.lib.func(
                "void* core_provider_cache_new()",
            );
            this.lib.core_provider_cache_free = this.lib.func(
                "void core_provider_cache_free(void*)",
            );
            this.lib.core_provider_cache_init_config = this.lib.func(
                "void core_provider_cache_init_config(void*, const char*, const char*, const char*, const char*, char*)",
            );
            this.lib.core_provider_cache_init_experiments = this.lib.func(
                "void core_provider_cache_init_experiments(void*, const char*, const char*, char*)",
            );
            this.lib.core_provider_cache_eval_config = this.lib.func(
                "char* core_provider_cache_eval_config(void*, const char*, const char*, const char*, const char*, const char*, char*)",
            );
            this.lib.core_provider_cache_filter_config = this.lib.func(
                "char* core_provider_cache_filter_config(void*, const char*, const char*, const char*, char*)",
            );
            this.lib.core_provider_cache_filter_experiment = this.lib.func(
                "char* core_provider_cache_filter_experiment(void*, const char*, const char*, const char*, bool, char*)",
            );
            this.lib.core_provider_cache_get_applicable_variants =
                this.lib.func(
                    "char* core_provider_cache_get_applicable_variants(void*, const char*, const char*, const char*, const char*, char*)",
                );

            this.isAvailable = true;
        } catch (error) {
            console.warn(
                "Native resolver library not available, falling back to JavaScript implementation:",
                error,
            );
            this.isAvailable = false;
        }
    }

    isNativeAvailable(): boolean {
        return this.isAvailable;
    }

    resolveConfig(
        defaultConfigs: Record<string, any>,
        contexts: Context[],
        overrides: Record<string, Record<string, any>>,
        dimensions: Record<string, DimensionInfo>,
        queryData: Record<string, any>,
        mergeStrategy: "merge" | "replace" = "merge",
        filterPrefixes?: string[],
        filterExcludePrefixes?: string[],
        experimentation?: any,
    ): Record<string, any> {
        if (!this.isAvailable) {
            throw new Error(
                "Native resolver is not available. Please ensure the native library is built and accessible.",
            );
        }

        // Input validation
        if (!contexts) {
            throw new Error("contexts parameter is required");
        }
        if (!overrides) {
            throw new Error("overrides parameter is required");
        }
        if (!dimensions) {
            throw new Error("dimensions parameter is required");
        }
        if (!queryData) {
            throw new Error("queryData parameter is required");
        }
        if (!mergeStrategy) {
            throw new Error("mergeStrategy parameter is required");
        }

        const defaultConfigsJson = JSON.stringify(defaultConfigs || {});
        const contextsJson = JSON.stringify(contexts);
        const overridesJson = JSON.stringify(overrides);
        const dimensionsJson = JSON.stringify(dimensions);
        const queryDataJson = JSON.stringify(queryData);
        const filterPrefixesJson =
            filterPrefixes && filterPrefixes.length > 0
                ? JSON.stringify(filterPrefixes)
                : null;
        const filterExcludePrefixesJson =
            filterExcludePrefixes && filterExcludePrefixes.length > 0
                ? JSON.stringify(filterExcludePrefixes)
                : null;
        const experimentationJson = experimentation
            ? JSON.stringify(experimentation)
            : null;

        console.log("🔧 Calling FFI with parameters:");
        console.log("  defaultConfigs:", defaultConfigs);
        console.log("  contexts length:", contextsJson.length);
        console.log("  overrides length:", overridesJson.length);
        console.log("  dimensions length:", dimensionsJson.length);
        console.log("  queryData :", queryDataJson);
        console.log("  mergeStrategy:", mergeStrategy);
        console.log("  filterPrefixes:", filterPrefixes);
        console.log("  experiment:", experimentation?.experiments?.length);
        console.log(
            "  experiment groups:",
            experimentation?.experiment_groups?.length,
        );
        console.log("  targetingKey:", experimentation?.targetingKey);

        if (
            !defaultConfigsJson ||
            defaultConfigsJson === "null" ||
            defaultConfigsJson === "undefined"
        ) {
            throw new Error("defaultConfigs serialization failed");
        }
        if (
            !contextsJson ||
            contextsJson === "null" ||
            contextsJson === "undefined"
        ) {
            throw new Error("contexts serialization failed");
        }
        if (
            !overridesJson ||
            overridesJson === "null" ||
            overridesJson === "undefined"
        ) {
            throw new Error("overrides serialization failed");
        }
        if (
            !dimensionsJson ||
            dimensionsJson === "null" ||
            dimensionsJson === "undefined"
        ) {
            throw new Error("dimensions serialization failed");
        }
        if (
            !queryDataJson ||
            queryDataJson === "null" ||
            queryDataJson === "undefined"
        ) {
            throw new Error("queryData serialization failed");
        }

        const ebuf = Buffer.alloc(ERROR_BUFFER_SIZE);
        const result = this.lib.core_get_resolved_config(
            defaultConfigsJson,
            contextsJson,
            overridesJson,
            dimensionsJson,
            queryDataJson,
            mergeStrategy,
            filterPrefixesJson,
            filterExcludePrefixesJson,
            experimentationJson,
            ebuf,
        );

        console.log("🔧 FFI call completed, result:", result);

        const err = ebuf.toString("utf8").split("\0")[0];
        if (err.length !== 0) {
            this.throwFFIError(err);
        }

        const configStr =
            typeof result === "string"
                ? result
                : this.lib.decode(result, "string");

        if (typeof result !== "string") {
            this.lib.core_free_string(result);
        }

        try {
            return JSON.parse(configStr);
        } catch (parseError) {
            console.error("Failed to parse config result:", parseError);
            console.error("Raw result string:", configStr);
            throw new Error(
                `Failed to parse config evaluation result: ${parseError}`,
            );
        }
    }

    getApplicableVariants(
        experiments: FfiExperiment[],
        experiment_groups: FfiExperimentGroup[],
        dimensions: Record<string, DimensionInfo>,
        userContext: Record<string, any>,
        identifier: string,
        filterPrefixes: string[] = [],
        filterExcludePrefixes: string[] = [],
    ): string[] {
        if (!this.isAvailable) {
            throw new Error(
                "Native resolver is not available. Please ensure the native library is built and accessible.",
            );
        }

        if (!experiments) {
            throw new Error("experiments parameter is required");
        }
        if (!userContext) {
            throw new Error("userContext parameter is required");
        }

        const experimentsJson = JSON.stringify(experiments);
        const experimentGroupsJson = JSON.stringify(experiment_groups);
        const userContextJson = JSON.stringify(userContext);
        const dimensionsJson = JSON.stringify(dimensions);
        const filterPrefixesJson =
            filterPrefixes.length > 0 ? JSON.stringify(filterPrefixes) : null;
        const filterExcludePrefixesJson =
            filterExcludePrefixes.length > 0
                ? JSON.stringify(filterExcludePrefixes)
                : null;

        console.log("Calling FFI getApplicableVariants with parameters:");
        console.log("  experiments:", experiments.length);
        console.log("  experimentGroups:", experiment_groups.length);
        console.log("  userContext:", userContext);
        console.log("  identifier:", identifier);
        console.log("  filterPrefixes:", filterPrefixes);
        console.log("  filterExcludePrefixes:", filterExcludePrefixes);

        const ebuf = Buffer.alloc(ERROR_BUFFER_SIZE);
        const result = this.lib.core_get_applicable_variants(
            experimentsJson,
            experimentGroupsJson,
            dimensionsJson,
            userContextJson,
            identifier,
            filterPrefixesJson,
            filterExcludePrefixesJson,
            ebuf,
        );

        console.log(
            "FFI getApplicableVariants call completed, result:",
            result,
        );

        const err = ebuf.toString("utf8").split("\0")[0];
        if (err.length !== 0) {
            this.throwFFIError(err);
        }

        const resultStr =
            typeof result === "string"
                ? result
                : this.lib.decode(result, "string");

        if (typeof result !== "string") {
            this.lib.core_free_string(result);
        }

        try {
            return JSON.parse(resultStr);
        } catch (parseError) {
            console.error("Failed to parse variants result:", parseError);
            console.error("Raw result string:", resultStr);
            throw new Error(
                `Failed to parse variants evaluation result: ${parseError}`,
            );
        }
    }

    /**
     * Parse a TOML/JSON config file and filter it by dimension data / prefixes in one step.
     *
     * Mirrors the Python/Java `parse_config_file_with_filters`; the file data source uses this to
     * honour context and prefix filtering at parse time. Passing no filters (the default) parses the
     * whole config, equivalent to a plain parse.
     *
     * @param fileContent - The file's contents.
     * @param format - `"toml"` or `"json"`.
     * @param dimensionData - Optional context to prune contexts/overrides by.
     * @param filterPrefixes - Optional key prefixes to include.
     * @param filterExcludePrefixes - Optional key prefixes to exclude.
     * @returns The filtered Config.
     * @throws Error if parsing fails.
     */
    parseConfigFileWithFilters(
        fileContent: string,
        format: "toml" | "json",
        dimensionData?: Record<string, any>,
        filterPrefixes?: string[],
        filterExcludePrefixes?: string[],
    ): Config {
        if (!this.isAvailable) {
            throw new Error(
                "Native resolver is not available. Please ensure the native library is built and accessible.",
            );
        }

        if (typeof fileContent !== "string") {
            throw new TypeError("fileContent must be a string");
        }

        const errorBuffer = Buffer.alloc(ERROR_BUFFER_SIZE);
        const dimensionDataJson =
            dimensionData && Object.keys(dimensionData).length > 0
                ? JSON.stringify(dimensionData)
                : null;
        const filterPrefixesJson =
            filterPrefixes && filterPrefixes.length > 0
                ? JSON.stringify(filterPrefixes)
                : null;
        const filterExcludePrefixesJson =
            filterExcludePrefixes && filterExcludePrefixes.length > 0
                ? JSON.stringify(filterExcludePrefixes)
                : null;

        const resultJson = this.lib.core_parse_config_file_with_filters(
            fileContent,
            format,
            dimensionDataJson,
            filterPrefixesJson,
            filterExcludePrefixesJson,
            errorBuffer,
        );

        if (!resultJson) {
            const nullTermIndex = errorBuffer.indexOf(0);
            const errorMsg = errorBuffer.toString(
                "utf8",
                0,
                nullTermIndex > 0 ? nullTermIndex : errorBuffer.length,
            );
            throw new Error(`Config parsing failed: ${errorMsg}`);
        }

        const configStr =
            typeof resultJson === "string"
                ? resultJson
                : this.lib.decode(resultJson, "string");

        if (typeof resultJson !== "string") {
            this.lib.core_free_string(resultJson);
        }

        try {
            return JSON.parse(configStr);
        } catch (parseError) {
            console.error("Failed to parse config result:", parseError);
            console.error("Raw result string:", configStr);
            throw new Error(`Failed to parse config result: ${parseError}`);
        }
    }

    /**
     * Get the path to the native library.
     * Uses the same approach as Java and Python - looks for GitHub artifacts first,
     * then falls back to local build.
     */
    private getDefaultLibPath(): string {
        const platform = os.platform();
        const arch = os.arch();

        let filename: string;
        let extension: string;

        // Determine file extension based on platform
        if (platform === "win32" && arch === "x64") {
            extension = "x86_64-pc-windows-msvc.dll";
        } else if (platform === "darwin" && arch === "arm64") {
            extension = "aarch64-apple-darwin.dylib";
        } else if (platform === "darwin" && arch === "x64") {
            extension = "x86_64-apple-darwin.dylib";
        } else {
            extension = "x86_64-unknown-linux-gnu.so";
        }

        filename = `libsuperposition_core-${extension}`;

        const dirname = path.dirname(fileURLToPath(import.meta.url));

        const packageRootPath = path.resolve(dirname, "..", filename);
        if (this.fileExists(packageRootPath)) {
            console.log(
                `Using native library from package root: ${packageRootPath}`,
            );
            return packageRootPath;
        }

        // 1. First try to load from package's native-lib directory (GitHub artifacts)
        const packageNativeLibPath = path.resolve(
            dirname,
            "native-lib",
            filename,
        );
        if (this.fileExists(packageNativeLibPath)) {
            console.log(
                `Using native library from package: ${packageNativeLibPath}`,
            );
            return packageNativeLibPath;
        }

        const packageNative2LibPath = path.resolve(
            dirname,
            "..",
            "native-lib",
            filename,
        );
        if (this.fileExists(packageNative2LibPath)) {
            console.log(
                `Using native library from package: ${packageNative2LibPath}`,
            );
            return packageNative2LibPath;
        }

        // 2. Try platform-specific subdirectory in native-lib
        const platformDir = `${platform}-${arch}`;
        const platformSpecificPath = path.resolve(
            dirname,
            "..",
            "native-lib",
            platformDir,
            filename,
        );
        if (this.fileExists(platformSpecificPath)) {
            console.log(
                `Using platform-specific native library: ${platformSpecificPath}`,
            );
            return platformSpecificPath;
        }

        // 3. Fall back to local build (relative to repository root)
        const localBuildPath = path.resolve(
            dirname,
            "..",
            "..",
            "..",
            "..",
            "target",
            "release",
            filename,
        );
        if (this.fileExists(localBuildPath)) {
            console.log(`Using local build: ${localBuildPath}`);
            return localBuildPath;
        }

        // 4. Try simple library name format (libsuperposition_core.dylib/so/dll)
        let simpleLibName: string;
        if (platform === "win32") {
            simpleLibName = "superposition_core.dll";
        } else if (platform === "darwin") {
            simpleLibName = "libsuperposition_core.dylib";
        } else {
            simpleLibName = "libsuperposition_core.so";
        }

        // 5. Final fallback - assume it's in the system path
        console.warn(
            `Native library not found in expected locations, trying: ${filename}`,
        );
        return filename;
    }

    private fileExists(filePath: string): boolean {
        try {
            return fs.existsSync(filePath);
        } catch {
            console.trace(`Binary not found for path ${filePath}`);
            return false;
        }
    }

    createProviderCache(): ProviderCache {
        if (!this.isAvailable) {
            throw new Error("Native resolver is not available.");
        }
        return new ProviderCache(this.lib);
    }

    private throwFFIError(err: String): never {
        throw new Error("ffi: " + err);
    }
}
