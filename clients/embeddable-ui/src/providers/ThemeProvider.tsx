import React, { useMemo } from "react";
import type {
  SuperpositionEmbeddableConfig,
  SuperpositionThemeMode,
  SuperpositionThemeTokens,
} from "../types";
import { ThemeContext, type SuperpositionThemeValue } from "./theme-context";

function resolveTheme(config: SuperpositionEmbeddableConfig): SuperpositionThemeValue {
  return {
    mode: config.theme?.mode ?? "light",
    tokens: config.theme,
  };
}

function buildThemeVars(
  mode: SuperpositionThemeMode,
  tokens?: SuperpositionThemeTokens,
): React.CSSProperties {
  const isDark = mode === "dark";

  return {
    "--sp-color-bg":
      tokens?.colorBg ?? (isDark ? "oklch(0.21 0.018 282)" : "oklch(0.982 0.007 286)"),
    "--sp-color-panel":
      tokens?.colorPanel ??
      (isDark ? "oklch(0.255 0.018 282)" : "oklch(0.995 0.004 286)"),
    "--sp-color-text":
      tokens?.colorText ?? (isDark ? "oklch(0.95 0.01 286)" : "oklch(0.29 0.022 282)"),
    "--sp-color-muted":
      tokens?.colorMuted ?? (isDark ? "oklch(0.72 0.014 282)" : "oklch(0.56 0.018 282)"),
    "--sp-color-border":
      tokens?.colorBorder ?? (isDark ? "oklch(0.34 0.016 282)" : "oklch(0.9 0.01 282)"),
    "--sp-color-primary": tokens?.colorPrimary ?? "oklch(0.59 0.19 282)",
    "--sp-color-primary-soft":
      "color-mix(in oklab, var(--sp-color-primary) 10%, var(--sp-color-panel))",
    "--sp-color-success": tokens?.colorSuccess ?? "oklch(0.69 0.16 154)",
    "--sp-color-warning": tokens?.colorWarning ?? "oklch(0.78 0.15 84)",
    "--sp-color-danger": tokens?.colorDanger ?? "oklch(0.62 0.2 25)",
    "--sp-shadow-sm":
      tokens?.shadowSm ??
      (isDark ? "0 10px 24px rgba(0, 0, 0, 0.22)" : "0 16px 40px rgba(41, 31, 78, 0.08)"),
    "--sp-shadow-md":
      tokens?.shadowMd ??
      (isDark ? "0 18px 42px rgba(0, 0, 0, 0.28)" : "0 24px 60px rgba(41, 31, 78, 0.12)"),
    "--sp-radius-sm": tokens?.radiusSm ?? "10px",
    "--sp-radius-md": tokens?.radiusMd ?? "14px",
    "--sp-radius-lg": tokens?.radiusLg ?? "22px",
    "--sp-space-xs": tokens?.spaceXs ?? "8px",
    "--sp-space-sm": tokens?.spaceSm ?? "12px",
    "--sp-space-md": tokens?.spaceMd ?? "16px",
    "--sp-space-lg": tokens?.spaceLg ?? "24px",
    "--sp-color-surface-muted":
      "color-mix(in oklab, var(--sp-color-panel) 88%, var(--sp-color-bg))",
    "--sp-color-surface-subtle":
      "color-mix(in oklab, var(--sp-color-panel) 72%, var(--sp-color-bg))",
    "--sp-color-overlay": isDark ? "rgba(8, 7, 14, 0.72)" : "rgba(22, 16, 40, 0.48)",
    "--sp-control-bg": "var(--sp-color-panel)",
    "--sp-control-text": "var(--sp-color-text)",
    "--sp-control-border": "var(--sp-color-border)",
    "--sp-control-radius": "var(--sp-radius-md)",
    "--sp-card-radius": "var(--sp-radius-lg)",
    "--sp-inline-radius": "var(--sp-radius-sm)",
    "--sp-pill-radius": "999px",
    "--sp-button-primary-bg": "var(--sp-color-primary)",
    "--sp-button-primary-text": "var(--sp-color-panel)",
    "--sp-button-primary-border": "transparent",
    "--sp-button-primary-shadow":
      "0 12px 24px color-mix(in oklab, var(--sp-color-primary) 18%, transparent)",
    "--sp-button-secondary-bg": "var(--sp-color-panel)",
    "--sp-button-secondary-text": "var(--sp-color-text)",
    "--sp-button-secondary-border": "var(--sp-color-border)",
    "--sp-button-danger-bg":
      "color-mix(in oklab, var(--sp-color-danger) 12%, var(--sp-color-panel))",
    "--sp-button-danger-text":
      "color-mix(in oklab, var(--sp-color-danger) 78%, var(--sp-color-text))",
    "--sp-button-danger-border":
      "color-mix(in oklab, var(--sp-color-danger) 28%, var(--sp-color-border))",
    "--sp-feedback-info-bg": "var(--sp-color-primary-soft)",
    "--sp-feedback-info-text": "var(--sp-color-text)",
    "--sp-feedback-info-border":
      "color-mix(in oklab, var(--sp-color-primary) 24%, var(--sp-color-border))",
    "--sp-feedback-success-bg":
      "color-mix(in oklab, var(--sp-color-success) 14%, var(--sp-color-panel))",
    "--sp-feedback-success-text":
      "color-mix(in oklab, var(--sp-color-success) 72%, var(--sp-color-text))",
    "--sp-feedback-success-border":
      "color-mix(in oklab, var(--sp-color-success) 32%, var(--sp-color-border))",
    "--sp-feedback-warning-bg":
      "color-mix(in oklab, var(--sp-color-warning) 18%, var(--sp-color-panel))",
    "--sp-feedback-warning-text":
      "color-mix(in oklab, var(--sp-color-warning) 76%, var(--sp-color-text))",
    "--sp-feedback-warning-border":
      "color-mix(in oklab, var(--sp-color-warning) 34%, var(--sp-color-border))",
    "--sp-feedback-danger-bg":
      "color-mix(in oklab, var(--sp-color-danger) 12%, var(--sp-color-panel))",
    "--sp-feedback-danger-text":
      "color-mix(in oklab, var(--sp-color-danger) 78%, var(--sp-color-text))",
    "--sp-feedback-danger-border":
      "color-mix(in oklab, var(--sp-color-danger) 32%, var(--sp-color-border))",
    "--sp-feedback-neutral-bg":
      "color-mix(in oklab, var(--sp-color-muted) 14%, var(--sp-color-panel))",
    "--sp-feedback-neutral-text": "var(--sp-color-text)",
    "--sp-feedback-neutral-border":
      "color-mix(in oklab, var(--sp-color-muted) 22%, var(--sp-color-border))",
    fontFamily:
      tokens?.fontFamily ??
      '-apple-system, BlinkMacSystemFont, "Segoe UI", Inter, Roboto, sans-serif',
    fontSize: tokens?.fontSize ?? "14px",
    color: "var(--sp-color-text)",
  } as React.CSSProperties;
}

export interface ThemeProviderProps {
  config: SuperpositionEmbeddableConfig;
  children: React.ReactNode;
}

export function ThemeProvider({ config, children }: ThemeProviderProps) {
  const value = useMemo(() => resolveTheme(config), [config]);
  const themeStyles = useMemo(
    () => buildThemeVars(value.mode, value.tokens),
    [value.mode, value.tokens],
  );

  return (
    <ThemeContext.Provider value={value}>
      <div className="sp-ui" data-sp-theme={value.mode} style={themeStyles}>
        {children}
      </div>
    </ThemeContext.Provider>
  );
}
