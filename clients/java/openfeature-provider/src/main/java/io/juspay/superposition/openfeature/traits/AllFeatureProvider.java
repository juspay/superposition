package io.juspay.superposition.openfeature.traits;

import dev.openfeature.sdk.EvaluationContext;
import dev.openfeature.sdk.ProviderEvaluation;
import dev.openfeature.sdk.Reason;
import dev.openfeature.sdk.Value;
import io.juspay.superposition.openfeature.error.SuperpositionError;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * Interface for bulk configuration resolution.
 *
 * This interface provides methods to resolve all feature flags at once,
 * which is more efficient than resolving them individually.
 *
 * Implementors should provide:
 * 1. resolveAllFeatures() - resolve all flags
 * 2. resolveAllFeaturesWithFilter() - resolve filtered flags
 *
 * Default implementations of typed helpers (resolveBool, resolveString, etc.) are provided,
 * so implementors only need to implement the two core methods.
 */
public interface AllFeatureProvider {

    /**
     * Resolve all features for the given evaluation context.
     *
     * @param context the evaluation context containing dimensions and targeting key
     * @return map of all feature keys to their resolved values (as JSON strings)
     * @throws SuperpositionError if resolution fails
     */
    default Map<String, String> resolveAllFeatures(EvaluationContext context) throws SuperpositionError {
        return resolveAllFeaturesWithFilter(context, Optional.empty());
    }
    /**
     * Resolve all features for the given context, optionally filtered by key prefixes.
     *
     * If prefix_filter is empty, behaves like resolveAllFeatures.
     * If prefix_filter is present, only returns features whose keys match any of the prefixes.
     *
     * @param context the evaluation context
     * @param prefixFilter optional list of key prefixes (empty = no filtering)
     * @return map of filtered feature keys to their resolved values
     * @throws SuperpositionError if resolution fails
     */
    Map<String, String> resolveAllFeaturesWithFilter(
            EvaluationContext context,
            Optional<List<String>> prefixFilter) throws SuperpositionError;

    /**
     * Default implementation of typed resolution with custom extractor.
     *
     * <p>TODO: successful resolutions leave {@code reason} unset. Reporting it accurately
     * (STATIC for a default-config value, TARGETING_MATCH for a context override, SPLIT for an
     * experiment variant) needs {@code eval_config} in {@code superposition_core} to say, per key,
     * where the value came from. Until it does, guessing would be worse than saying nothing —
     * a flag no experiment touched would still get labelled SPLIT. Error reasons are set below.
     * The same TODO applies to the Rust and Python clients.
     *
     * @param flagKey the flag key to resolve
     * @param context the evaluation context
     * @param typeName name of the type (for error messages)
     * @param extractor function to extract the value and convert to type T
     * @param <T> the type of the resolved value
     * @return ProviderEvaluation containing the resolved value
     */
    default <T> ProviderEvaluation<T> resolveTyped(
            String flagKey,
            EvaluationContext context,
            String typeName,
            java.util.function.Function<Object, Optional<T>> extractor) {
        try {
            Map<String, String> config = resolveAllFeatures(context);
            String valueStr = config.get(flagKey);

            if (valueStr == null) {
                return ProviderEvaluation.<T>builder()
                        .variant("default")
                        .reason(Reason.ERROR.name())
                        .errorCode(dev.openfeature.sdk.ErrorCode.FLAG_NOT_FOUND)
                        .errorMessage("Flag '" + flagKey + "' not found")
                        .build();
            }

            Object value = JsonValues.parse(valueStr);
            Optional<T> extracted = extractor.apply(value);

            if (extracted.isPresent()) {
                return ProviderEvaluation.<T>builder()
                        .value(extracted.get())
                        .variant("evaluated")
                        .build();
            } else {
                return ProviderEvaluation.<T>builder()
                        .variant("default")
                        .reason(Reason.ERROR.name())
                        .errorCode(dev.openfeature.sdk.ErrorCode.TYPE_MISMATCH)
                        .errorMessage("Flag '" + flagKey + "' is not a " + typeName)
                        .build();
            }
        } catch (SuperpositionError e) {
            return ProviderEvaluation.<T>builder()
                    .variant("error")
                    .reason(Reason.ERROR.name())
                    .errorCode(dev.openfeature.sdk.ErrorCode.GENERAL)
                    .errorMessage("Error evaluating flag '" + flagKey + "': " + e.getMessage())
                    .build();
        }
    }

    /**
     * Resolve a boolean flag.
     *
     * @param flagKey the flag key
     * @param context the evaluation context
     * @return ProviderEvaluation with the boolean value
     */
    default ProviderEvaluation<Boolean> resolveBool(
            String flagKey,
            EvaluationContext context) {
        return resolveTyped(flagKey, context, "boolean",
                value -> {
                    if (value instanceof Boolean) return Optional.of((Boolean) value);
                    return Optional.empty();
                });
    }

    /**
     * Resolve a string flag.
     *
     * @param flagKey the flag key
     * @param context the evaluation context
     * @return ProviderEvaluation with the string value
     */
    default ProviderEvaluation<String> resolveString(
            String flagKey,
            EvaluationContext context) {
        return resolveTyped(flagKey, context, "string",
                value -> {
                    if (value instanceof String) return Optional.of((String) value);
                    return Optional.empty();
                });
    }

    /**
     * Resolve an integer flag.
     *
     * @param flagKey the flag key
     * @param context the evaluation context
     * @return ProviderEvaluation with the integer value
     */
    default ProviderEvaluation<Integer> resolveInt(
            String flagKey,
            EvaluationContext context) {
        return resolveTyped(flagKey, context, "integer",
                value -> {
                    // Only a JSON integer literal that fits an int. A Double here means the flag
                    // holds 1.9, 10.0, or a value too large for an int — truncating any of those
                    // would return a value the flag does not hold.
                    if (value instanceof Integer number) {
                        return Optional.of(number);
                    }
                    return Optional.empty();
                });
    }

    /**
     * Resolve a floating-point flag.
     *
     * @param flagKey the flag key
     * @param context the evaluation context
     * @return ProviderEvaluation with the double value
     */
    default ProviderEvaluation<Double> resolveFloat(
            String flagKey,
            EvaluationContext context) {
        return resolveTyped(flagKey, context, "float",
                value -> {
                    // Integers widen to float: every integer is a real number, so this is
                    // lossless. The reverse (float -> int) is not, and is rejected above.
                    if (value instanceof Integer || value instanceof Double) {
                        return Optional.of(((Number) value).doubleValue());
                    }
                    return Optional.empty();
                });
    }

    /**
     * Resolve a structured (object) flag.
     *
     * @param flagKey the flag key
     * @param context the evaluation context
     * @return ProviderEvaluation with the object value
     */
    default ProviderEvaluation<Value> resolveStruct(
            String flagKey,
            EvaluationContext context) {
        return resolveTyped(flagKey, context, "object",
                value -> {
                    // Objects and arrays only. The guard is the point: objectToValue also accepts
                    // a String, a Boolean and a Number, so calling it unguarded meant a string
                    // flag read via getObjectValue succeeded instead of reporting TYPE_MISMATCH.
                    if (value instanceof Map || value instanceof List) {
                        return Optional.of(Value.objectToValue(value));
                    }
                    return Optional.empty();
                });
    }

}
