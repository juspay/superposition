import type { FeatureMountOptions, SuperpositionHostAdapters } from "../browser-runtime";
import type { SuperpositionAdminProps } from "../pages/SuperpositionAdmin";
import { getBrowserCoreGlobal } from "./global-core";

export const customElementTagName = "superposition-admin";

export function mount(
  container: Element | string,
  options: FeatureMountOptions<SuperpositionAdminProps>,
) {
  return getBrowserCoreGlobal().mountSuperpositionFeature(container, "admin", options);
}

export function defineCustomElement(prefix = "superposition") {
  return getBrowserCoreGlobal().defineFeatureCustomElement("admin", prefix);
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
