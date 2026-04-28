import React from "react";
import { createRoot, type Root } from "react-dom/client";
import {
  AuditLog,
  ConfigManager,
  DimensionManager,
  ExperimentManager,
  OverrideManager,
  ResolveView,
  SuperpositionAdmin,
} from "./pages";
import { AlertProvider, SuperpositionProvider } from "./providers";
import stylesText from "./styles.css?inline";
import type { SuperpositionEmbeddableConfig } from "./types";

type FeatureName =
  | "admin"
  | "config-manager"
  | "override-manager"
  | "dimension-manager"
  | "experiment-manager"
  | "resolve-view"
  | "audit-log";

type FeatureComponentProps = Record<string, unknown>;

const featureComponents: Record<
  FeatureName,
  React.ComponentType<FeatureComponentProps>
> = {
  admin: SuperpositionAdmin,
  "config-manager": ConfigManager,
  "override-manager": OverrideManager,
  "dimension-manager": DimensionManager,
  "experiment-manager": ExperimentManager,
  "resolve-view": ResolveView,
  "audit-log": AuditLog,
};

export const customElementTagNames: Record<FeatureName, string> = {
  admin: "superposition-admin",
  "config-manager": "superposition-config-manager",
  "override-manager": "superposition-override-manager",
  "dimension-manager": "superposition-dimension-manager",
  "experiment-manager": "superposition-experiment-manager",
  "resolve-view": "superposition-resolve-view",
  "audit-log": "superposition-audit-log",
};

function isTruthy(value: string | null): boolean | undefined {
  if (value === null) return undefined;
  return value === "" || value === "true" || value === "1";
}

function parseConfigValue(element: HTMLElement): SuperpositionEmbeddableConfig | null {
  const configAttr = element.getAttribute("config");
  const parsedConfig = configAttr
    ? (JSON.parse(configAttr) as Partial<SuperpositionEmbeddableConfig>)
    : {};
  const baseUrl = element.getAttribute("base-url");
  const orgId = element.getAttribute("org-id") ?? parsedConfig.orgId;
  const workspace = element.getAttribute("workspace") ?? parsedConfig.workspace;
  const themeMode = element.getAttribute("theme-mode");
  const readOnly = isTruthy(element.getAttribute("read-only"));

  const resolvedBaseUrl = baseUrl ?? parsedConfig.apiBaseUrl;

  if (!orgId || !workspace || !resolvedBaseUrl) {
    return null;
  }

  return {
    ...parsedConfig,
    apiBaseUrl: resolvedBaseUrl,
    orgId,
    workspace,
    readOnly: readOnly ?? parsedConfig.readOnly,
    theme: themeMode
      ? {
          ...(typeof parsedConfig.theme === "object" ? parsedConfig.theme : {}),
          mode: themeMode as "light" | "dark" | "system",
        }
      : parsedConfig.theme,
  } as SuperpositionEmbeddableConfig;
}

function renderFeature(
  container: Element,
  feature: FeatureName,
  config: SuperpositionEmbeddableConfig,
  props: FeatureComponentProps = {},
): Root {
  const FeatureComponent = featureComponents[feature];
  const root = createRoot(container);
  root.render(
    <React.StrictMode>
      <div className="sp-ui">
        <SuperpositionProvider config={config}>
          <AlertProvider>
            <FeatureComponent {...props} />
          </AlertProvider>
        </SuperpositionProvider>
      </div>
    </React.StrictMode>,
  );
  return root;
}

export function mountSuperpositionFeature(
  container: Element | string,
  feature: FeatureName,
  options: { config: SuperpositionEmbeddableConfig; props?: FeatureComponentProps },
): { unmount: () => void } {
  const resolvedContainer =
    typeof container === "string" ? document.querySelector(container) : container;

  if (!resolvedContainer) {
    throw new Error("Unable to find mount container for Superposition UI.");
  }

  const root = renderFeature(resolvedContainer, feature, options.config, options.props);

  return {
    unmount: () => root.unmount(),
  };
}

class SuperpositionFeatureElement extends HTMLElement {
  static observedAttributes = [
    "config",
    "org-id",
    "workspace",
    "base-url",
    "theme-mode",
    "read-only",
  ];

  private root?: Root;
  private mountNode?: HTMLDivElement;
  private styleNode?: HTMLStyleElement;

  constructor(private readonly feature: FeatureName) {
    super();
  }

  connectedCallback() {
    this.ensureShadowRoot();
    this.renderFeature();
  }

  disconnectedCallback() {
    this.root?.unmount();
    this.root = undefined;
  }

  attributeChangedCallback() {
    if (this.isConnected) {
      this.renderFeature();
    }
  }

  private ensureShadowRoot() {
    if (!this.shadowRoot) {
      this.attachShadow({ mode: "open" });
    }

    if (!this.styleNode) {
      this.styleNode = document.createElement("style");
      this.styleNode.textContent = `:host{display:block;} ${stylesText}`;
      this.shadowRoot!.appendChild(this.styleNode);
    }

    if (!this.mountNode) {
      this.mountNode = document.createElement("div");
      this.shadowRoot!.appendChild(this.mountNode);
    }
  }

  private renderFeature() {
    if (!this.mountNode) return;

    try {
      const config = parseConfigValue(this);
      if (!config) {
        this.root?.unmount();
        this.root = undefined;
        this.mountNode.replaceChildren();
        return;
      }

      this.root?.unmount();
      this.root = renderFeature(this.mountNode, this.feature, config);
    } catch (error) {
      this.root?.unmount();
      this.root = undefined;
      this.mountNode.replaceChildren();

      const errorNode = document.createElement("div");
      errorNode.setAttribute("role", "alert");
      errorNode.style.padding = "12px";
      errorNode.style.borderRadius = "var(--sp-inline-radius, 10px)";
      errorNode.style.border =
        "1px solid var(--sp-feedback-danger-border, oklch(0.82 0.08 25))";
      errorNode.style.background = "var(--sp-feedback-danger-bg, oklch(0.96 0.03 25))";
      errorNode.style.color = "var(--sp-feedback-danger-text, oklch(0.46 0.17 25))";
      errorNode.style.fontSize = "13px";
      errorNode.textContent =
        error instanceof Error ? error.message : "Failed to mount Superposition UI.";
      this.mountNode.appendChild(errorNode);
    }
  }
}

function createFeatureElement(feature: FeatureName) {
  return class extends SuperpositionFeatureElement {
    constructor() {
      super(feature);
    }
  };
}

export function defineCustomElements(prefix = "superposition") {
  const tagMap: Record<FeatureName, string> = {
    admin: `${prefix}-admin`,
    "config-manager": `${prefix}-config-manager`,
    "override-manager": `${prefix}-override-manager`,
    "dimension-manager": `${prefix}-dimension-manager`,
    "experiment-manager": `${prefix}-experiment-manager`,
    "resolve-view": `${prefix}-resolve-view`,
    "audit-log": `${prefix}-audit-log`,
  };

  (Object.keys(tagMap) as FeatureName[]).forEach((feature) => {
    const tagName = tagMap[feature];
    if (!customElements.get(tagName)) {
      customElements.define(tagName, createFeatureElement(feature));
    }
  });

  return tagMap;
}
