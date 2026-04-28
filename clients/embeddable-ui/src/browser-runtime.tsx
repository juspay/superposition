import React from "react";
import { createRoot, type Root } from "react-dom/client";
import { AlertProvider, SuperpositionUIProvider } from "./providers";
import stylesText from "./styles.css?inline";
import type {
  SuperpositionAuthConfig,
  SuperpositionEmbeddableConfig,
  SuperpositionNetworkHooks,
  SuperpositionUiAdapters,
} from "./types";

export type FeatureName =
  | "admin"
  | "config-manager"
  | "override-manager"
  | "dimension-manager";

type FeatureComponent<TProps extends object> =
  | React.ComponentType<TProps>
  | React.LazyExoticComponent<React.ComponentType<TProps>>;

const HOST_ADAPTER_REGISTRY_KEY = "__SUPERPOSITION_HOST_ADAPTERS__";

export interface FeatureMountOptions<TProps extends object> {
  config: SuperpositionEmbeddableConfig;
  props?: TProps;
}

export interface SuperpositionHostAdapters {
  auth?: SuperpositionAuthConfig;
  network?: SuperpositionNetworkHooks;
  ui?: SuperpositionUiAdapters;
}

function getHostAdapterRegistry(): Map<string, SuperpositionHostAdapters> {
  const globalScope = globalThis as typeof globalThis & {
    __SUPERPOSITION_HOST_ADAPTERS__?: Map<string, SuperpositionHostAdapters>;
  };

  if (!globalScope[HOST_ADAPTER_REGISTRY_KEY]) {
    globalScope[HOST_ADAPTER_REGISTRY_KEY] = new Map<string, SuperpositionHostAdapters>();
  }

  return globalScope[HOST_ADAPTER_REGISTRY_KEY];
}

const hostAdapterRegistry = getHostAdapterRegistry();

export function registerSuperpositionHostAdapters(
  id: string,
  adapters: SuperpositionHostAdapters,
) {
  hostAdapterRegistry.set(id, adapters);
}

export function unregisterSuperpositionHostAdapters(id: string) {
  hostAdapterRegistry.delete(id);
}

export function createTagName(prefix: string, suffix: string) {
  return `${prefix}-${suffix}`;
}

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
  const strict = isTruthy(element.getAttribute("strict"));
  const adapterId = element.getAttribute("adapter-id");
  const adapters = adapterId ? hostAdapterRegistry.get(adapterId) : undefined;

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
    strict: strict ?? parsedConfig.strict,
    auth: adapters?.auth ?? parsedConfig.auth,
    network: adapters?.network ?? parsedConfig.network,
    ui: {
      ...(parsedConfig.ui ?? {}),
      ...(adapters?.ui ?? {}),
    },
    theme: themeMode
      ? {
        ...(typeof parsedConfig.theme === "object" ? parsedConfig.theme : {}),
        mode: themeMode as "light" | "dark" | "system",
      }
      : parsedConfig.theme,
  } as SuperpositionEmbeddableConfig;
}

function renderFeature<TProps extends object>(
  container: Element,
  FeatureComponent: FeatureComponent<TProps>,
  config: SuperpositionEmbeddableConfig,
  props?: TProps,
): Root {
  const root = createRoot(container);
  const featureProps = (props ?? {}) as TProps;

  root.render(
    <React.StrictMode>
      <div className="sp-ui">
        <SuperpositionUIProvider config={config}>
          <AlertProvider>
            <React.Suspense
              fallback={
                <div
                  role="status"
                  aria-live="polite"
                  style={{
                    padding: "12px",
                    color: "var(--sp-color-muted)",
                    fontSize: "13px",
                  }}
                >
                  Loading Superposition UI...
                </div>
              }
            >
              {React.createElement(
                FeatureComponent as React.ComponentType<TProps>,
                featureProps,
              )}
            </React.Suspense>
          </AlertProvider>
        </SuperpositionUIProvider>
      </div>
    </React.StrictMode>,
  );

  return root;
}

export function mountFeatureComponent<TProps extends object>(
  container: Element | string,
  FeatureComponent: FeatureComponent<TProps>,
  options: FeatureMountOptions<TProps>,
): { unmount: () => void } {
  const resolvedContainer =
    typeof container === "string" ? document.querySelector(container) : container;

  if (!resolvedContainer) {
    throw new Error("Unable to find mount container for Superposition UI.");
  }

  const root = renderFeature(
    resolvedContainer,
    FeatureComponent,
    options.config,
    options.props,
  );

  return {
    unmount: () => root.unmount(),
  };
}

function createFeatureElement<TProps extends object>(
  FeatureComponent: FeatureComponent<TProps>,
) {
  return class extends HTMLElement {
    static observedAttributes = [
      "config",
      "org-id",
      "workspace",
      "base-url",
      "theme-mode",
      "read-only",
      "strict",
      "adapter-id",
    ];

    private root?: Root;
    private mountNode?: HTMLDivElement;
    private styleNode?: HTMLStyleElement;

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
        this.root = renderFeature(this.mountNode, FeatureComponent, config);
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
  };
}

export function defineSingleCustomElement<TProps extends object>(
  tagName: string,
  FeatureComponent: FeatureComponent<TProps>,
) {
  if (!customElements.get(tagName)) {
    customElements.define(tagName, createFeatureElement(FeatureComponent));
  }

  return tagName;
}
