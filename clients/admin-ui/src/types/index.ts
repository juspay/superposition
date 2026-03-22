export * from "./api";
import type { JsonValue } from "./api";

/** Configuration for the SuperpositionProvider */
export interface SuperpositionConfig {
  /** Base URL of the Superposition server (e.g. "https://superposition.example.com") */
  host: string;
  /** Organisation ID */
  orgId: string;
  /** Workspace name */
  workspace: string;
  /**
   * Optional scoped context: a map of dimension names to values.
   * When set, the UI will:
   * - Filter overrides/experiments to those matching this context
   * - Pre-fill these dimensions when creating new overrides
   * - Optionally lock these dimensions so they cannot be changed
   */
  context?: Record<string, JsonValue>;
  /** Authentication configuration */
  auth?: {
    /** Bearer token or custom token string */
    token?: string;
    /** Custom headers to add to every request */
    headers?: Record<string, string>;
  };
  /** Restrict which feature pages are available */
  features?: Array<
    "config" | "overrides" | "dimensions" | "experiments" | "resolve" | "audit"
  >;
  /** Theme (defaults to "light") */
  theme?: "light" | "dark";
  /** If true, scoped context dimensions are locked (non-editable) in forms */
  lockScopedDimensions?: boolean;
}
