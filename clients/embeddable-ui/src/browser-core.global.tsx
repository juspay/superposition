import type { ComponentType } from "react";
import {
  createTagName,
  defineSingleCustomElement,
  mountFeatureComponent,
  registerSuperpositionHostAdapters,
  unregisterSuperpositionHostAdapters,
  type FeatureName,
} from "./browser-runtime";
import { ConfigManager } from "./pages/ConfigManager";
import { DimensionManager } from "./pages/DimensionManager";
import { OverrideManager } from "./pages/OverrideManager";
import { SuperpositionAdmin } from "./pages/SuperpositionAdmin";
import type { SuperpositionEmbeddableConfig } from "./types";

type FeatureComponentProps = object;

const featureComponents = {
  admin: SuperpositionAdmin,
  "config-manager": ConfigManager,
  "override-manager": OverrideManager,
  "dimension-manager": DimensionManager,
} satisfies Record<FeatureName, ComponentType<any>>;

const featureTagSuffixes: Record<FeatureName, string> = {
  admin: "admin",
  "config-manager": "config-manager",
  "override-manager": "override-manager",
  "dimension-manager": "dimension-manager",
};

export const customElementTagNames: Record<FeatureName, string> = {
  admin: createTagName("superposition", featureTagSuffixes.admin),
  "config-manager": createTagName("superposition", featureTagSuffixes["config-manager"]),
  "override-manager": createTagName("superposition", featureTagSuffixes["override-manager"]),
  "dimension-manager": createTagName(
    "superposition",
    featureTagSuffixes["dimension-manager"],
  ),
};

export function mountSuperpositionFeature(
  container: Element | string,
  feature: FeatureName,
  options: { config: SuperpositionEmbeddableConfig; props?: FeatureComponentProps },
) {
  return mountFeatureComponent(
    container,
    featureComponents[feature] as ComponentType<FeatureComponentProps>,
    options,
  );
}

export function defineFeatureCustomElement(
  feature: FeatureName,
  prefix = "superposition",
) {
  const tagName = createTagName(prefix, featureTagSuffixes[feature]);
  return defineSingleCustomElement(
    tagName,
    featureComponents[feature] as ComponentType<FeatureComponentProps>,
  );
}

export function defineCustomElements(prefix = "superposition") {
  const tagMap = Object.fromEntries(
    (Object.keys(featureComponents) as FeatureName[]).map((feature) => [
      feature,
      defineFeatureCustomElement(feature, prefix),
    ]),
  ) as Record<FeatureName, string>;

  return tagMap;
}

export {
  registerSuperpositionHostAdapters,
  unregisterSuperpositionHostAdapters,
};
