import React from "react";
import type { ComponentType } from "react";
import {
  createTagName,
  defineSingleCustomElement,
  mountFeatureComponent,
  registerSuperpositionHostAdapters,
  unregisterSuperpositionHostAdapters,
  type FeatureName,
  type SuperpositionHostAdapters,
} from "./browser-runtime";
import type { SuperpositionEmbeddableConfig } from "./types";

type FeatureComponentProps = Record<string, unknown>;

function lazyFeature(
  load: () => Promise<{ default: ComponentType<FeatureComponentProps> }>,
) {
  return React.lazy(load);
}

const featureComponents: Record<FeatureName, ReturnType<typeof lazyFeature>> = {
  admin: React.lazy(async () => {
    const mod = await import("./pages/SuperpositionAdmin");
    return { default: mod.SuperpositionAdmin as React.ComponentType<FeatureComponentProps> };
  }),
  "config-manager": React.lazy(async () => {
    const mod = await import("./pages/ConfigManager");
    return { default: mod.ConfigManager as React.ComponentType<FeatureComponentProps> };
  }),
  "override-manager": React.lazy(async () => {
    const mod = await import("./pages/OverrideManager");
    return { default: mod.OverrideManager as React.ComponentType<FeatureComponentProps> };
  }),
  "dimension-manager": React.lazy(async () => {
    const mod = await import("./pages/DimensionManager");
    return { default: mod.DimensionManager as React.ComponentType<FeatureComponentProps> };
  }),
};

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
): { unmount: () => void } {
  return mountFeatureComponent(container, featureComponents[feature], options);
}

export function defineCustomElements(prefix = "superposition") {
  const tagMap = Object.fromEntries(
    (Object.keys(featureComponents) as FeatureName[]).map((feature) => [
      feature,
      createTagName(prefix, featureTagSuffixes[feature]),
    ]),
  ) as Record<FeatureName, string>;

  (Object.keys(tagMap) as FeatureName[]).forEach((feature) => {
    const tagName = tagMap[feature];
    defineSingleCustomElement(tagName, featureComponents[feature]);
  });

  return tagMap;
}

export { registerSuperpositionHostAdapters, unregisterSuperpositionHostAdapters };
export type { SuperpositionHostAdapters };
