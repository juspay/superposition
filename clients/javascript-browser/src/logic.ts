/**
 * Logic module for applying conditions to context
 * JavaScript/TypeScript equivalent of the Rust logic module
 */

/**
 * Represents a JSON value that can be a string, number, boolean, object, array, or null
 */
type JsonValue = string | number | boolean | object | any[] | null;

/**
 * Represents a map/object with string keys and JsonValue values
 */
type JsonMap = { [key: string]: JsonValue };

/**
 * Internal function that applies logic for both partial and full condition matching
 * @param condition - The condition map to check against
 * @param context - The context map to match
 * @param partial - Whether to allow partial matching (skip missing context keys)
 * @returns true if the condition matches, false otherwise
 */
function applyLogic(
    condition: JsonMap,
    context: JsonMap,
    partial: boolean
): boolean {
    for (const [dimension, value] of Object.entries(condition)) {
        if (dimension in context) {
            const contextValue = context[dimension];

            if (dimension === "variantIds") {
                if (Array.isArray(contextValue)) {
                    if (!contextValue.includes(value)) {
                        return false;
                    }
                } else {
                    return false;
                }
            } else if (!deepEqual(contextValue, value)) {
                return false;
            }
        } else if (partial) {
            continue;
        } else {
            return false;
        }
    }
    return true;
}

/**
 * Deep equality comparison for JSON values
 * @param a - First value to compare
 * @param b - Second value to compare
 * @returns true if values are deeply equal, false otherwise
 */
function deepEqual(a: JsonValue, b: JsonValue): boolean {
    // Check if the types of a and b are the same
    if (typeof a !== typeof b) {
        return false;
    }

    // If both a and b are objects (including arrays and null), compare their properties
    if (typeof a === "object") {
        // Handle null case
        if (a === null || b === null) {
            return a === b;
        }

        // Handle arrays
        if (Array.isArray(a) && Array.isArray(b)) {
            if (a.length !== b.length) {
                return false;
            }
            for (let i = 0; i < a.length; i++) {
                if (!deepEqual(a[i], b[i])) {
                    return false;
                }
            }
            return true;
        }

        // Handle objects
        const keysA = Object.keys(a);
        const keysB = Object.keys(b);
        if (keysA.length !== keysB.length) {
            return false;
        }

        // Recursively compare each property
        for (const key of keysA) {
            if (!deepEqual((a as JsonMap)[key], (b as JsonMap)[key])) {
                return false;
            }
        }
        return true;
    }

    // For other types (string, number, boolean), compare the values directly
    return a === b;
}

/**
 * Applies a condition to a context with full matching
 * All dimensions in the condition must have corresponding values in the context
 * @param condition - The condition map to check against
 * @param context - The context map to match
 * @returns true if the condition fully matches the context, false otherwise
 */
export function apply(condition: JsonMap, context: JsonMap): boolean {
    return applyLogic(condition, context, false);
}

/**
 * Applies a condition to a context with partial matching
 * Missing context keys are ignored, only present keys are checked
 * @param condition - The condition map to check against
 * @param context - The context map to match
 * @returns false if any condition mismatches
 */
export function partialApply(condition: JsonMap, context: JsonMap): boolean {
    return applyLogic(condition, context, true);
}

export default {
    apply,
    partialApply,
};
