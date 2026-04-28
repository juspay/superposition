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
