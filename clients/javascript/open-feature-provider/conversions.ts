/**
 * Conversions from SDK response shapes to the FFI shapes the native cache consumes.
 *
 * Mirrors Python's `conversions.py`, but simpler: the JS SDK hands back plain JS values (its
 * `Document` type is just JSON), so there is no Smithy-`Document` unwrapping to do. Config values
 * stay nested (the FFI parses `default_configs` as a JSON value map); experiment `context`/`overrides`
 * are flattened to string maps (the FFI parses them as a `Condition`, i.e. `Map<String,String>`),
 * matching the shape the existing `ExperimentationClient` already feeds the cache.
 */

import {
    GroupType,
    VariantType,
    ExperimentStatusType,
} from "superposition-sdk";
import type {
    Config,
    ExperimentConfig,
    Variant,
    FfiExperiment,
    FfiExperimentGroup,
} from "superposition-bindings";

/**
 * Normalize any value into a `Record<string, string>`, JSON-stringifying non-string values. This is
 * the `Condition` shape the FFI expects for experiment contexts and variant overrides.
 */
export function normalizeToStringRecord(value: any): Record<string, string> {
    const result: Record<string, string> = {};
    if (value == null) {
        return result;
    }
    if (typeof value === "object" && !Array.isArray(value)) {
        for (const [key, val] of Object.entries(value)) {
            if (typeof val === "string") {
                result[key] = val;
            } else if (val != null) {
                result[key] = JSON.stringify(val);
            }
        }
    }
    return result;
}

/**
 * Build the FFI {@link Config} from a `GetConfigOutput`. The four fields pass straight through: the
 * FFI parses `default_configs` as a value map and `contexts`/`overrides`/`dimensions` as their
 * respective structures, and the SDK's JSON shapes already line up (as the legacy client proved).
 */
export function configResponseToFfiConfig(response: any): Config {
    return {
        default_configs: response.default_configs || {},
        contexts: response.contexts || [],
        overrides: response.overrides || {},
        dimensions: response.dimensions || {},
    };
}

/** Convert a list of SDK `ExperimentResponse` into the FFI experiment shape. */
export function experimentsToFfiExperiments(
    experiments: any[] | undefined,
): FfiExperiment[] {
    if (!experiments) {
        return [];
    }
    const result: FfiExperiment[] = [];
    for (const exp of experiments) {
        if (!exp.id) {
            continue;
        }
        const variants: Variant[] = [];
        for (const variant of exp.variants || []) {
            if (!variant.id) {
                continue;
            }
            const variantType =
                variant.variant_type === VariantType.CONTROL
                    ? VariantType.CONTROL
                    : VariantType.EXPERIMENTAL;
            variants.push({
                id: variant.id,
                variant_type: variantType,
                context_id: variant.context_id,
                override_id: variant.override_id,
                overrides: normalizeToStringRecord(variant.overrides),
            });
        }
        result.push({
            id: exp.id,
            context: normalizeToStringRecord(exp.context),
            variants,
            traffic_percentage: exp.traffic_percentage ?? 100,
            status: exp.status || ExperimentStatusType.DISCARDED,
        });
    }
    return result;
}

/** Convert a list of SDK `ExperimentGroupResponse` into the FFI experiment-group shape. */
export function expGrpsToFfiExpGrps(
    groups: any[] | undefined,
): FfiExperimentGroup[] {
    if (!groups) {
        return [];
    }
    const result: FfiExperimentGroup[] = [];
    for (const group of groups) {
        if (!group.id) {
            continue;
        }
        result.push({
            id: group.id,
            context: normalizeToStringRecord(group.context),
            traffic_percentage: group.traffic_percentage ?? 100,
            member_experiment_ids: group.member_experiment_ids || [],
            group_type:
                (group.group_type as GroupType) || GroupType.USER_CREATED,
            buckets: (group.buckets || []).map((bucket: any) => ({
                variant_id: bucket?.variant_id || "",
                experiment_id: bucket?.experiment_id || "",
            })),
        });
    }
    return result;
}

/** Build an FFI {@link ExperimentConfig} from SDK experiment + group responses. */
export function experimentConfigFromResponses(
    experiments: any[] | undefined,
    groups: any[] | undefined,
): ExperimentConfig {
    return {
        experiments: experimentsToFfiExperiments(experiments),
        experiment_groups: expGrpsToFfiExpGrps(groups),
    };
}
