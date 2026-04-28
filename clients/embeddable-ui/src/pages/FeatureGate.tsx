import type {
  SuperpositionEmbeddableConfig,
  SuperpositionFeature,
  SuperpositionFeatureCapabilities,
} from "../types";

export function isFeatureEnabled(
  features: SuperpositionFeature[] | undefined,
  feature: SuperpositionFeature,
): boolean {
  return !features || features.includes(feature);
}

export function canUseFeatureAction(
  config: SuperpositionEmbeddableConfig,
  feature: SuperpositionFeature,
  action: keyof SuperpositionFeatureCapabilities,
): boolean {
  if (config.readOnly && action !== "execute") {
    return false;
  }

  return config.capabilities?.[feature]?.[action] ?? true;
}

export function getMessage(
  config: SuperpositionEmbeddableConfig,
  key: string,
  fallback: string,
  values?: Record<string, string | number>,
): string {
  const template = config.messages?.[key] ?? fallback;
  if (!values) return template;

  return Object.entries(values).reduce(
    (result, [name, value]) => result.split(`{${name}}`).join(String(value)),
    template,
  );
}

export function FeatureUnavailable({
  feature,
  message,
}: {
  feature: string;
  message?: string;
}) {
  return (
    <div
      role="status"
      style={{
        border: "1px solid var(--sp-color-border)",
        borderRadius: "var(--sp-card-radius)",
        background: "var(--sp-color-surface-muted)",
        color: "var(--sp-color-muted)",
        padding: "var(--sp-space-lg)",
        fontSize: "1rem",
      }}
    >
      {message ?? `${feature} is not enabled for this embed.`}
    </div>
  );
}
