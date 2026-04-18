/**
 * Format an ISO datetime string to a user-friendly local string.
 */
export function formatDateTime(iso: string): string {
  try {
    return new Date(iso).toLocaleString();
  } catch {
    return iso;
  }
}

/**
 * Format a JSON value for display, truncating if too long.
 */
export function formatJsonPreview(value: unknown, maxLength = 80): string {
  const str = JSON.stringify(value);
  if (str.length <= maxLength) return str;
  return str.slice(0, maxLength - 3) + "...";
}

/**
 * Build condition description from a context map.
 * e.g. { region: "us-east-1", env: "prod" } → "region = us-east-1, env = prod"
 */
export function describeCondition(
  condition: Record<string, unknown>,
): string {
  return Object.entries(condition)
    .map(([k, v]) => `${k} = ${JSON.stringify(v)}`)
    .join(", ");
}
