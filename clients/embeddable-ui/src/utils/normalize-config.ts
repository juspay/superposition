import type {
  NormalizedSuperpositionConfig,
  SuperpositionEmbeddableConfig,
} from "../types";

function trimTrailingSlashes(value: string): string {
  return value.replace(/\/+$/, "");
}

function inferTransportMode(
  baseUrl: string,
): NormalizedSuperpositionConfig["transport"]["mode"] {
  if (!baseUrl || baseUrl.startsWith("/")) {
    return "host-proxy";
  }

  if (typeof window !== "undefined") {
    try {
      return new URL(baseUrl, window.location.href).origin === window.location.origin
        ? "same-origin"
        : "cross-origin";
    } catch {
      return "cross-origin";
    }
  }

  return "cross-origin";
}

export function normalizeSuperpositionConfig(
  config: SuperpositionEmbeddableConfig,
): NormalizedSuperpositionConfig {
  const { apiBaseUrl, apiBasePath, credentials, workspaceHeaderName, scope, ...rest } =
    config;

  return {
    ...rest,
    transport: {
      mode: inferTransportMode(apiBaseUrl),
      baseUrl: trimTrailingSlashes(apiBaseUrl),
      apiBasePath,
      credentials: credentials ?? "include",
      workspaceHeaderName: workspaceHeaderName ?? "x-workspace",
    },
    context: scope?.context,
    lockScopedDimensions: scope?.locked ?? true,
    routing: config.routing ?? { mode: "internal" },
  };
}
