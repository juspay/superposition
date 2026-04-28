import type { FeatureName, SuperpositionHostAdapters } from "../browser-runtime";
import type { SuperpositionEmbeddableConfig } from "../types";

export interface BrowserCoreGlobalApi {
  customElementTagNames: Record<FeatureName, string>;
  mountSuperpositionFeature: <TProps extends object>(
    container: Element | string,
    feature: FeatureName,
    options: { config: SuperpositionEmbeddableConfig; props?: TProps },
  ) => { unmount: () => void };
  defineCustomElements: (prefix?: string) => Record<FeatureName, string>;
  defineFeatureCustomElement: (feature: FeatureName, prefix?: string) => string;
  registerSuperpositionHostAdapters: (
    id: string,
    adapters: SuperpositionHostAdapters,
  ) => void;
  unregisterSuperpositionHostAdapters: (id: string) => void;
}

export function getBrowserCoreGlobal(): BrowserCoreGlobalApi {
  const core = (globalThis as typeof globalThis & {
    SuperpositionBrowserCore?: BrowserCoreGlobalApi;
  }).SuperpositionBrowserCore;

  if (!core) {
    throw new Error(
      "SuperpositionBrowserCore is not loaded. Load superposition-browser-core.global.external.js first.",
    );
  }

  return core;
}
