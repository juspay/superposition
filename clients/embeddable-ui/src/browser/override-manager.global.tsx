import type { FeatureMountOptions, SuperpositionHostAdapters } from "../browser-runtime";
import type { OverrideManagerProps } from "../pages/OverrideManager";
import { getBrowserCoreGlobal } from "./global-core";

export const customElementTagName = "superposition-override-manager";

export function mount(
  container: Element | string,
  options: FeatureMountOptions<OverrideManagerProps>,
) {
  return getBrowserCoreGlobal().mountSuperpositionFeature(
    container,
    "override-manager",
    options,
  );
}

export function defineCustomElement(prefix = "superposition") {
  return getBrowserCoreGlobal().defineFeatureCustomElement("override-manager", prefix);
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
