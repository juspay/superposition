/**
 * Shared resolution contract for the Superposition providers.
 *
 * `AllFeatureProvider` resolves all features once, extracts a single flag, and applies the unified
 * type-coercion contract (FLAG_NOT_FOUND / TYPE_MISMATCH / GENERAL). Experiment-variant resolution is
 * a separate concern, kept in its own {@link FeatureExperimentMeta} interface to match the reference
 * clients' trait split.
 * OpenFeature JS resolvers may be async, so there is a single async path.
 *
 * The FFI hands back real typed values (numbers, booleans, objects, arrays), so the extractors test
 * the JS type directly rather than decoding per key.
 */

import {
    EvaluationContext,
    ErrorCode,
    JsonValue,
    Logger,
    OpenFeatureEventEmitter,
    ProviderMetadata,
    ResolutionDetails,
    StandardResolutionReasons,
    Hook,
} from "@openfeature/server-sdk";

/** Extracts a typed value from a resolved flag value, or `undefined` on a type mismatch. */
type Extractor<T> = (value: any) => T | undefined;

const asBoolean: Extractor<boolean> = (v) =>
    typeof v === "boolean" ? v : undefined;
const asString: Extractor<string> = (v) =>
    typeof v === "string" ? v : undefined;
// `typeof true === "boolean"`, so booleans never leak in as numbers.
const asNumber: Extractor<number> = (v) =>
    typeof v === "number" && !Number.isNaN(v) ? v : undefined;
// Arrays and non-null objects both count as "object" (arrays allowed, matching Python/JS).
const asObject: Extractor<any> = (v) =>
    typeof v === "object" && v !== null ? v : undefined;

/**
 * Experiment-variant resolution. This is a capability distinct from feature resolution — the two are
 * separate traits in the Rust client (`FeatureExperimentMeta` vs `AllFeatureProvider`) and separate
 * ABCs in Python — so it lives in its own interface here rather than being folded into the base class.
 * Concrete providers implement it alongside extending {@link AllFeatureProvider}.
 */
export interface FeatureExperimentMeta {
    /** Get the applicable experiment variant IDs for the given context. */
    getApplicableVariants(
        context: EvaluationContext,
        prefixFilter?: string[],
        excludePrefixFilter?: string[],
    ): Promise<string[]>;
}

/**
 * The bulk-resolution contract (the Rust trait / Python ABC / Java interface of the same name):
 * resolve all features once, extract a single flag, and apply the unified type-coercion contract. It
 * supplies the shared OpenFeature `resolve*Evaluation` implementations and provider plumbing, but does
 * not itself declare `implements Provider` — it is not a usable provider on its own (no
 * `initialize`/`status`). The concrete providers extend this, declare `implements Provider`, and
 * separately implement {@link FeatureExperimentMeta} — mirroring the reference clients, where
 * `AllFeatureProvider` is a distinct trait/mixin from the OpenFeature provider it is combined into.
 */
export abstract class AllFeatureProvider {
    abstract readonly metadata: ProviderMetadata;

    readonly runsOn = "server" as const;
    events = new OpenFeatureEventEmitter();
    readonly hooks: Hook[] = [];

    /**
     * Resolve all features for the given context, optionally filtered by key prefixes. Concrete
     * providers implement this (locally via the FFI cache, or remotely via the API).
     */
    abstract resolveAllFeaturesWithFilter(
        context: EvaluationContext,
        prefixFilter?: string[],
        excludePrefixFilter?: string[],
    ): Promise<Record<string, any>>;

    /** Resolve all features for the given context (no prefix filter). */
    resolveAllFeatures(
        context: EvaluationContext,
    ): Promise<Record<string, any>> {
        return this.resolveAllFeaturesWithFilter(context, undefined, undefined);
    }

    /**
     * Resolve all features, extract `flagKey`, and apply `extractor`.
     *
     * `reason` is left unset on success — reporting it accurately (STATIC / TARGETING_MATCH / SPLIT)
     * needs the core to say, per key, where a value came from; until it does, guessing would be worse
     * than saying nothing. The same TODO applies to the Rust/Python/Java clients.
     */
    protected async resolveTyped<T>(
        flagKey: string,
        context: EvaluationContext,
        typeName: string,
        extractor: Extractor<T>,
        defaultValue: T,
        logger?: Logger,
    ): Promise<ResolutionDetails<T>> {
        try {
            const config = await this.resolveAllFeatures(context);
            if (!(flagKey in config)) {
                return {
                    value: defaultValue,
                    reason: StandardResolutionReasons.ERROR,
                    errorCode: ErrorCode.FLAG_NOT_FOUND,
                    errorMessage: `Flag '${flagKey}' not found`,
                };
            }
            const extracted = extractor(config[flagKey]);
            if (extracted === undefined) {
                return {
                    value: defaultValue,
                    reason: StandardResolutionReasons.ERROR,
                    errorCode: ErrorCode.TYPE_MISMATCH,
                    errorMessage: `Flag '${flagKey}' is not a ${typeName}`,
                };
            }
            return { value: extracted };
        } catch (error) {
            // Log the evaluation failure through OpenFeature's logger (the SDK passes one per call),
            // mirroring the reference clients which log it in resolve_typed before returning GENERAL.
            const message = `Error evaluating ${typeName} flag '${flagKey}': ${
                error instanceof Error ? error.message : String(error)
            }`;
            logger?.error(message);
            return {
                value: defaultValue,
                reason: StandardResolutionReasons.ERROR,
                errorCode: ErrorCode.GENERAL,
                errorMessage: message,
            };
        }
    }

    // --- OpenFeature FeatureProvider methods ---

    resolveBooleanEvaluation(
        flagKey: string,
        defaultValue: boolean,
        context: EvaluationContext,
        logger?: Logger,
    ): Promise<ResolutionDetails<boolean>> {
        return this.resolveTyped(
            flagKey,
            context,
            "boolean",
            asBoolean,
            defaultValue,
            logger,
        );
    }

    resolveStringEvaluation(
        flagKey: string,
        defaultValue: string,
        context: EvaluationContext,
        logger?: Logger,
    ): Promise<ResolutionDetails<string>> {
        return this.resolveTyped(
            flagKey,
            context,
            "string",
            asString,
            defaultValue,
            logger,
        );
    }

    resolveNumberEvaluation(
        flagKey: string,
        defaultValue: number,
        context: EvaluationContext,
        logger?: Logger,
    ): Promise<ResolutionDetails<number>> {
        return this.resolveTyped(
            flagKey,
            context,
            "number",
            asNumber,
            defaultValue,
            logger,
        );
    }

    resolveObjectEvaluation<T extends JsonValue>(
        flagKey: string,
        defaultValue: T,
        context: EvaluationContext,
        logger?: Logger,
    ): Promise<ResolutionDetails<T>> {
        return this.resolveTyped<T>(
            flagKey,
            context,
            "object",
            asObject,
            defaultValue,
            logger,
        );
    }
}
