export function normalizeFilterValues(values?: string | string[]): string[] | undefined {
  if (!values) {
    return undefined;
  }

  const normalized = (Array.isArray(values) ? values : [values])
    .map((item) => item.trim())
    .filter(Boolean);

  return normalized.length > 0 ? normalized : undefined;
}

export function matchesPrefix(value: string, prefixes?: string[]): boolean {
  if (!prefixes || prefixes.length === 0) {
    return true;
  }

  return prefixes.some((prefix) => value.startsWith(prefix));
}

export function filterRecordByPrefix<T>(
  values: Record<string, T>,
  prefixes?: string[],
): Record<string, T> {
  if (!prefixes || prefixes.length === 0) {
    return values;
  }

  return Object.fromEntries(
    Object.entries(values).filter(([key]) => matchesPrefix(key, prefixes)),
  );
}
