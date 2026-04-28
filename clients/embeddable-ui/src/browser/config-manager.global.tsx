import type { FeatureMountOptions, SuperpositionHostAdapters } from "../browser-runtime";
import type { ConfigManagerProps } from "../pages/ConfigManager";
import { getBrowserCoreGlobal } from "./global-core";

export const customElementTagName = "superposition-config-manager";

export function mount(
  container: Element | string,
  options: FeatureMountOptions<ConfigManagerProps>,
) {
  return getBrowserCoreGlobal().mountSuperpositionFeature(
    container,
    "config-manager",
    options,
  );
}

export function defineCustomElement(prefix = "superposition") {
  return getBrowserCoreGlobal().defineFeatureCustomElement("config-manager", prefix);
}

export function registerSuperpositionHostAdapters(
  id: string,
  adapters: SuperpositionHostAdapters,
) {
  return getBrowserCoreGlobal().registerSuperpositionHostAdapters(id, adapters);
}

export function unregisterSuperpositionHostAdapters(id: string) {
  return getBrowserCoreGlobal().unregisterSuperpositionHostAdapters(id);
}
