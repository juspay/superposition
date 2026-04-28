import type React from "react";
import {
  createTagName,
  defineSingleCustomElement,
  mountFeatureComponent,
  type FeatureMountOptions,
} from "../browser-runtime";

export function createFeatureGlobal<TProps extends object>(
  suffix: string,
  FeatureComponent: React.ComponentType<TProps>,
) {
  return {
    customElementTagName: createTagName("superposition", suffix),
    mount(container: Element | string, options: FeatureMountOptions<TProps>) {
      return mountFeatureComponent(container, FeatureComponent, options);
    },
    defineCustomElement(prefix = "superposition") {
      return defineSingleCustomElement(createTagName(prefix, suffix), FeatureComponent);
    },
  };
}
