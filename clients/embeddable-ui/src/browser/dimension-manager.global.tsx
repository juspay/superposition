import type { FeatureMountOptions, SuperpositionHostAdapters } from "../browser-runtime";
import type { DimensionManagerProps } from "../pages/DimensionManager";
import { getBrowserCoreGlobal } from "./global-core";

export const customElementTagName = "superposition-dimension-manager";

export function mount(
  container: Element | string,
  options: FeatureMountOptions<DimensionManagerProps>,
) {
  return getBrowserCoreGlobal().mountSuperpositionFeature(
    container,
    "dimension-manager",
    options,
  );
}

export function defineCustomElement(prefix = "superposition") {
  return getBrowserCoreGlobal().defineFeatureCustomElement("dimension-manager", prefix);
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
