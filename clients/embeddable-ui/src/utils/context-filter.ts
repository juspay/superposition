import type { Condition, ContextOverride, JsonValue } from "../types";

/**
 * Scoped context type: a map of dimension names to values.
 */
export type ScopedContext = Record<string, JsonValue>;

/**
 * Check if a context (condition map) matches the scoped context.
 * A match means every key in scopedContext appears in the condition
 * with the same value (or the condition value contains the scoped value).
 */
export function contextMatchesScope(
  condition: Condition,
  scopedContext: ScopedContext,
): boolean {
  for (const [key, scopedValue] of Object.entries(scopedContext)) {
    const conditionValue = condition[key];
    if (conditionValue === undefined) {
      return false;
    }

    // Direct equality
    if (JSON.stringify(conditionValue) === JSON.stringify(scopedValue)) continue;

    // If condition value is an array (IN operator), check if scoped value is included
    if (Array.isArray(conditionValue) && conditionValue.includes(scopedValue)) continue;

    // The condition has this dimension but with a different value → no match
    return false;
  }
  return true;
}

function jsonValueEquals(left: JsonValue, right: JsonValue): boolean {
  return JSON.stringify(left) === JSON.stringify(right);
}

export function contextCanBeEditedInScope(
  condition: Condition,
  scopedContext?: ScopedContext,
): boolean {
  if (!scopedContext || Object.keys(scopedContext).length === 0) {
    return false;
  }

  const conditionEntries = Object.entries(condition);
  if (conditionEntries.length === 0) {
    return false;
  }

  return conditionEntries.every(([key, conditionValue]) => {
    if (!(key in scopedContext)) return false;
    return jsonValueEquals(conditionValue, scopedContext[key]);
  });
}

/**
 * Filter overrides to only those matching the scoped context.
 */
export function filterOverridesByScope(
  overrides: ContextOverride[],
  scopedContext?: ScopedContext,
): ContextOverride[] {
  if (!scopedContext || Object.keys(scopedContext).length === 0) {
    return overrides;
  }
  return overrides.filter((o) => contextMatchesScope(o.value, scopedContext));
}

/**
 * Merge scoped context dimensions into a new context (for creating overrides).
 * Scoped dimensions take precedence.
 */
export function mergeScopedContext(
  userContext: Condition,
  scopedContext?: ScopedContext,
): Condition {
  if (!scopedContext) return userContext;
  return { ...userContext, ...scopedContext };
}

export function stripScopedDimensions(
  userContext: Condition,
  scopedContext?: ScopedContext,
): { context: Condition; removedKeys: string[] } {
  if (!scopedContext || Object.keys(scopedContext).length === 0) {
    return { context: userContext, removedKeys: [] };
  }

  const removedKeys: string[] = [];
  const context = Object.fromEntries(
    Object.entries(userContext).filter(([key]) => {
      const isLocked = key in scopedContext;
      if (isLocked) {
        removedKeys.push(key);
      }
      return !isLocked;
    }),
  );

  return { context, removedKeys };
}

/**
 * Get the list of dimension names that are locked (from scoped context).
 */
export function getLockedDimensions(scopedContext?: ScopedContext): string[] {
  if (!scopedContext) return [];
  return Object.keys(scopedContext);
}
