import React, { useEffect, useMemo, useState } from "react";
import ReactDOM from "react-dom/client";
import { AlertProvider, SuperpositionAdmin, SuperpositionUIProvider } from "../src/index";
import type {
  JsonValue,
  SuperpositionEmbeddableConfig,
  SuperpositionFeature,
  SuperpositionThemeConfig,
  SuperpositionThemeMode,
} from "../src/types";
import { SUPERPOSITION_FEATURE_LABELS, SUPERPOSITION_FEATURES } from "../src/types";

interface DemoErrorBoundaryProps {
  children: React.ReactNode;
}

interface DemoErrorBoundaryState {
  error: string | null;
}

type DemoStatusTone = "error" | "success" | "info" | "idle";

interface DemoDraftState {
  host: string;
  orgId: string;
  workspace: string;
  token: string;
  features: string;
  allowOverrideContextEditing: boolean;
  scopeJson: string;
  writeScopeJson: string;
  scopeLocked: boolean;
  strict: boolean;
  themeMode: SuperpositionThemeMode;
  themePrimary: string;
  themeBg: string;
  themePanel: string;
  buttonBg: string;
  buttonText: string;
  iconColor: string;
  searchBorder: string;
  dropdownSelectedBg: string;
  bannerBg: string;
}

interface DemoPersistedState extends DemoDraftState {
  feature: SuperpositionFeature;
  connected: boolean;
  activeConnection?: DemoDraftState;
}

const STORAGE_KEY = "sp-embeddable-demo-config";
const DEFAULT_FEATURE: SuperpositionFeature = "config";
const DEFAULT_DRAFT: DemoDraftState = {
  host: "/api",
  orgId: "localorg",
  workspace: "dev",
  token: "",
  features: "",
  allowOverrideContextEditing: false,
  scopeJson: "",
  writeScopeJson: "",
  scopeLocked: true,
  strict: false,
  themeMode: "system",
  themePrimary: "#4f46e5",
  themeBg: "#f3f4f6",
  themePanel: "#ffffff",
  buttonBg: "#7c3aed",
  buttonText: "#ffffff",
  iconColor: "#4b5563",
  searchBorder: "#d1d5db",
  dropdownSelectedBg: "#f3f4f6",
  bannerBg: "#fff7ed",
};

class DemoErrorBoundary extends React.Component<
  DemoErrorBoundaryProps,
  DemoErrorBoundaryState
> {
  constructor(props: DemoErrorBoundaryProps) {
    super(props);
    this.state = { error: null };
  }

  static getDerivedStateFromError(error: unknown): DemoErrorBoundaryState {
    return {
      error: error instanceof Error ? error.message : String(error),
    };
  }

  render() {
    if (this.state.error) {
      return (
        <div id="status" className="error" style={{ display: "block" }}>
          Demo failed to render: {this.state.error}. Clear the saved demo state and
          reload.
        </div>
      );
    }

    return this.props.children;
  }
}

function isSuperpositionFeature(value: string): value is SuperpositionFeature {
  return SUPERPOSITION_FEATURES.includes(value as SuperpositionFeature);
}

function parseFeatureList(value: string): SuperpositionFeature[] | undefined {
  const features = value
    .split(",")
    .map((item) => item.trim())
    .filter(isSuperpositionFeature);

  return features.length > 0 ? features : undefined;
}

function serializeFeatureList(features: readonly SuperpositionFeature[]): string {
  return features.join(",");
}

function readFeatureFromUrl(): SuperpositionFeature {
  const params = new URLSearchParams(window.location.search);
  const feature = params.get("feature");
  if (feature && isSuperpositionFeature(feature)) {
    return feature;
  }
  return DEFAULT_FEATURE;
}

function writeFeatureToUrl(feature: SuperpositionFeature) {
  const url = new URL(window.location.href);
  url.searchParams.set("feature", feature);
  window.history.replaceState({}, "", url);
}

function parseOptionalJsonObject(value: string): Record<string, JsonValue> | undefined {
  const trimmed = value.trim();
  if (!trimmed) return undefined;

  const parsed = JSON.parse(trimmed) as unknown;
  if (!parsed || Array.isArray(parsed) || typeof parsed !== "object") {
    throw new Error(
      'Scoped filter must be a JSON object, for example {"region":"us-east-1"}.',
    );
  }

  return parsed as Record<string, JsonValue>;
}

function readPersistedState(): DemoPersistedState | null {
  const raw = window.localStorage.getItem(STORAGE_KEY);
  if (!raw) return null;

  try {
    const parsed = JSON.parse(raw) as Partial<DemoPersistedState> | null;
    if (!parsed || typeof parsed !== "object") {
      throw new Error("Saved state is not an object.");
    }

    const feature = isSuperpositionFeature(String(parsed.feature))
      ? (parsed.feature as SuperpositionFeature)
      : DEFAULT_FEATURE;
    const themeMode =
      parsed.themeMode === "light" ||
      parsed.themeMode === "dark" ||
      parsed.themeMode === "system"
        ? parsed.themeMode
        : DEFAULT_DRAFT.themeMode;

    const normalized: DemoPersistedState = {
      ...DEFAULT_DRAFT,
      ...parsed,
      feature,
      themeMode,
      connected: Boolean(parsed.connected),
    };

    if (typeof normalized.scopeJson !== "string") {
      normalized.scopeJson = DEFAULT_DRAFT.scopeJson;
    }
    if (typeof normalized.writeScopeJson !== "string") {
      normalized.writeScopeJson = DEFAULT_DRAFT.writeScopeJson;
    }
    normalized.strict = Boolean(normalized.strict);

    if (
      normalized.activeConnection &&
      (typeof normalized.activeConnection !== "object" ||
        Array.isArray(normalized.activeConnection))
    ) {
      normalized.activeConnection = undefined;
    }

    return normalized;
  } catch {
    window.localStorage.removeItem(STORAGE_KEY);
    return null;
  }
}

function buildTheme(state: DemoDraftState): SuperpositionThemeConfig {
  return {
    mode: state.themeMode,
    colors: {
      primary: state.themePrimary || undefined,
      bg: state.themeBg || undefined,
      panel: state.themePanel || undefined,
    },
    button: {
      primary: {
        bgColor: state.buttonBg || undefined,
        textColor: state.buttonText || undefined,
      },
    },
    icon: {
      color: state.iconColor || undefined,
    },
    search: {
      borderColor: state.searchBorder || undefined,
      icon: {
        color: state.iconColor || undefined,
      },
    },
    dropdown: {
      option: {
        selectedBgColor: state.dropdownSelectedBg || undefined,
      },
    },
    banner: {
      bgColor: state.bannerBg || undefined,
    },
  };
}

function buildConfig(
  state: DemoDraftState,
  feature: SuperpositionFeature,
): SuperpositionEmbeddableConfig {
  const host = state.host.replace(/\/+$/, "");
  const features = parseFeatureList(state.features);

  return {
    apiBaseUrl: host,
    orgId: state.orgId,
    workspace: state.workspace,
    auth: state.token ? { mode: "bearer", token: state.token } : undefined,
    capabilities: state.allowOverrideContextEditing
      ? {
          overrides: {
            editContext: true,
          },
        }
      : undefined,
    scope: {
      context: parseOptionalJsonObject(state.scopeJson),
      writeContext: parseOptionalJsonObject(state.writeScopeJson),
      locked: state.scopeLocked,
    },
    strict: state.strict,
    features,
    theme: buildTheme(state),
    routing: {
      mode: "external",
      currentFeature: feature,
      initialFeature: feature,
      onNavigate: () => {},
      getFeatureHref: () => "",
    },
  };
}

function DemoApp() {
  const persisted = useMemo(() => readPersistedState(), []);
  const [draft, setDraft] = useState<DemoDraftState>(persisted ?? DEFAULT_DRAFT);
  const [activeConnection, setActiveConnection] = useState<DemoDraftState | null>(
    persisted?.connected ? (persisted.activeConnection ?? persisted) : null,
  );
  const [activeFeature, setActiveFeature] =
    useState<SuperpositionFeature>(readFeatureFromUrl());
  const [status, setStatus] = useState<{ tone: DemoStatusTone; message: string }>({
    tone: "idle",
    message: "",
  });
  const [isConnecting, setIsConnecting] = useState(false);

  const apiMode = draft.host.trim().startsWith("/") ? "host proxy" : "direct API";
  const selectedFeatures = parseFeatureList(draft.features);
  const allFeaturesSelected = !selectedFeatures;

  const handleFeatureChange = (feature: SuperpositionFeature, checked: boolean) => {
    const current = selectedFeatures ?? [...SUPERPOSITION_FEATURES];
    const next = checked
      ? Array.from(new Set([...current, feature]))
      : current.filter((item) => item !== feature);

    handleFieldChange(
      "features",
      next.length === SUPERPOSITION_FEATURES.length
        ? ""
        : serializeFeatureList(next.length > 0 ? next : [feature]),
    );
  };

  useEffect(() => {
    if (!new URLSearchParams(window.location.search).get("feature")) {
      writeFeatureToUrl(activeFeature);
    }
  }, [activeFeature]);

  useEffect(() => {
    window.localStorage.setItem(
      STORAGE_KEY,
      JSON.stringify({
        ...draft,
        feature: activeFeature,
        connected: Boolean(activeConnection),
        activeConnection: activeConnection ?? undefined,
      } satisfies DemoPersistedState),
    );
  }, [draft, activeFeature, activeConnection]);

  useEffect(() => {
    const onPopState = () => {
      setActiveFeature(readFeatureFromUrl());
    };

    window.addEventListener("popstate", onPopState);
    return () => window.removeEventListener("popstate", onPopState);
  }, []);

  const configState = useMemo<{
    config: SuperpositionEmbeddableConfig | null;
    error: string | null;
  }>(() => {
    if (!activeConnection) {
      return { config: null, error: null };
    }

    try {
      const baseConfig = buildConfig(activeConnection, activeFeature);
      const routing = "routing" in baseConfig ? baseConfig.routing : undefined;

      return {
        config: {
          ...baseConfig,
          routing: routing
            ? {
                ...routing,
                currentFeature: activeFeature,
                initialFeature: activeFeature,
                onNavigate: (feature) => {
                  setActiveFeature(feature);
                  writeFeatureToUrl(feature);
                },
                getFeatureHref: (feature) => {
                  const url = new URL(window.location.href);
                  url.searchParams.set("feature", feature);
                  return url.pathname + url.search;
                },
              }
            : undefined,
        },
        error: null,
      };
    } catch (error) {
      return {
        config: null,
        error: error instanceof Error ? error.message : String(error),
      };
    }
  }, [activeConnection, activeFeature]);

  const config = configState?.config ?? null;

  useEffect(() => {
    if (configState?.error) {
      setStatus({
        tone: "error",
        message: `Cannot build demo config — ${configState.error}`,
      });
    }
  }, [configState]);

  const handleFieldChange = <K extends keyof DemoDraftState>(
    key: K,
    value: DemoDraftState[K],
  ) => {
    setDraft((current) => ({ ...current, [key]: value }));
  };

  const connect = async () => {
    const host = draft.host.replace(/\/+$/, "");
    if (!host || !draft.orgId || !draft.workspace) {
      setStatus({ tone: "error", message: "Host, Org ID, and Workspace are required." });
      setActiveConnection(null);
      return;
    }

    try {
      parseOptionalJsonObject(draft.scopeJson);
    } catch (error) {
      setStatus({
        tone: "error",
        message: error instanceof Error ? error.message : "Invalid scope JSON.",
      });
      return;
    }

    setIsConnecting(true);
    setStatus({ tone: "info", message: `Connecting to ${host} ...` });

    try {
      const resp = await fetch(host + "/health");
      if (!resp.ok) throw new Error("HTTP " + resp.status);
      setStatus({ tone: "success", message: `Connected to ${host}` });
    } catch (err) {
      const message = err instanceof Error ? err.message : String(err);
      setStatus({
        tone: "error",
        message:
          `Cannot reach ${host}/health — ${message}. ` +
          "Make sure the Superposition server is running on localhost:8080.",
      });
    } finally {
      setActiveConnection({ ...draft });
      setIsConnecting(false);
    }
  };

  useEffect(() => {
    if (persisted?.connected) {
      void connect();
    }
  }, []);

  return (
    <>
      <div className="toolbar">
        <div className="toolbar-links">
          <a href="./custom-elements.html">Open custom element demo</a>
          <a href="./shared-core-feature-globals.html">Open shared-core global demo</a>
        </div>

        <label htmlFor="host">Host:</label>
        <input
          id="host"
          type="text"
          value={draft.host}
          onChange={(event) => handleFieldChange("host", event.target.value)}
          style={{ width: 220 }}
          title="Use /api to go through Vite proxy (avoids CORS)"
        />

        <span className="toolbar-chip">API: {apiMode}</span>

        <label htmlFor="org">Org ID:</label>
        <input
          id="org"
          type="text"
          value={draft.orgId}
          onChange={(event) => handleFieldChange("orgId", event.target.value)}
          style={{ width: 120 }}
        />

        <label htmlFor="workspace">Workspace:</label>
        <input
          id="workspace"
          type="text"
          value={draft.workspace}
          onChange={(event) => handleFieldChange("workspace", event.target.value)}
          style={{ width: 120 }}
        />

        <label htmlFor="token">Token:</label>
        <input
          id="token"
          type="password"
          value={draft.token}
          onChange={(event) => handleFieldChange("token", event.target.value)}
          placeholder="(optional)"
          style={{ width: 150 }}
        />

        <span style={{ fontSize: 13, fontWeight: 500, color: "#374151" }}>Features:</span>
        <label style={{ display: "inline-flex", alignItems: "center", gap: 6 }}>
          <input
            type="checkbox"
            checked={allFeaturesSelected}
            onChange={(event) =>
              handleFieldChange("features", event.target.checked ? "" : "config")
            }
          />
          All
        </label>
        {SUPERPOSITION_FEATURES.map((feature) => (
          <label
            key={feature}
            style={{ display: "inline-flex", alignItems: "center", gap: 6 }}
          >
            <input
              type="checkbox"
              checked={
                allFeaturesSelected || Boolean(selectedFeatures?.includes(feature))
              }
              onChange={(event) => handleFeatureChange(feature, event.target.checked)}
            />
            {SUPERPOSITION_FEATURE_LABELS[feature]}
          </label>
        ))}

        <label style={{ display: "inline-flex", alignItems: "center", gap: 6 }}>
          <input
            type="checkbox"
            checked={draft.allowOverrideContextEditing}
            onChange={(event) =>
              handleFieldChange("allowOverrideContextEditing", event.target.checked)
            }
          />
          Override Context Editing
        </label>

        <label htmlFor="scope-json">Scoped Filter:</label>
        <input
          id="scope-json"
          type="text"
          value={draft.scopeJson}
          onChange={(event) => handleFieldChange("scopeJson", event.target.value)}
          placeholder='{"region":"us-east-1"}'
          style={{ width: 220 }}
          title="Optional bounded context for the embedded UI"
        />

        <label htmlFor="write-scope-json">Write Scope:</label>
        <input
          id="write-scope-json"
          type="text"
          value={draft.writeScopeJson}
          onChange={(event) => handleFieldChange("writeScopeJson", event.target.value)}
          placeholder='{"merchant_id":"m_123","profile_id":"p_123"}'
          style={{ width: 260 }}
          title="Optional narrower context for create and edit actions"
        />

        <label htmlFor="lock-scoped-dimensions">Lock Scope:</label>
        <input
          id="lock-scoped-dimensions"
          type="checkbox"
          checked={draft.scopeLocked}
          onChange={(event) => handleFieldChange("scopeLocked", event.target.checked)}
        />

        <label htmlFor="strict-scope">Strict:</label>
        <input
          id="strict-scope"
          type="checkbox"
          checked={draft.strict}
          onChange={(event) => handleFieldChange("strict", event.target.checked)}
        />

        <label htmlFor="theme-mode">Theme Mode:</label>
        <select
          id="theme-mode"
          value={draft.themeMode}
          onChange={(event) =>
            handleFieldChange("themeMode", event.target.value as SuperpositionThemeMode)
          }
        >
          <option value="system">system</option>
          <option value="light">light</option>
          <option value="dark">dark</option>
        </select>

        <label htmlFor="theme-primary">Primary:</label>
        <input
          id="theme-primary"
          type="color"
          value={draft.themePrimary}
          onChange={(event) => handleFieldChange("themePrimary", event.target.value)}
        />

        <label htmlFor="theme-bg">Background:</label>
        <input
          id="theme-bg"
          type="color"
          value={draft.themeBg}
          onChange={(event) => handleFieldChange("themeBg", event.target.value)}
        />

        <label htmlFor="theme-panel">Panel:</label>
        <input
          id="theme-panel"
          type="color"
          value={draft.themePanel}
          onChange={(event) => handleFieldChange("themePanel", event.target.value)}
        />

        <label htmlFor="button-bg">Button:</label>
        <input
          id="button-bg"
          type="color"
          value={draft.buttonBg}
          onChange={(event) => handleFieldChange("buttonBg", event.target.value)}
          title="theme.button.primary.bgColor"
        />

        <label htmlFor="icon-color">Icons:</label>
        <input
          id="icon-color"
          type="color"
          value={draft.iconColor}
          onChange={(event) => handleFieldChange("iconColor", event.target.value)}
          title="theme.icon.color"
        />

        <label htmlFor="search-border">Search Border:</label>
        <input
          id="search-border"
          type="color"
          value={draft.searchBorder}
          onChange={(event) => handleFieldChange("searchBorder", event.target.value)}
          title="theme.search.borderColor"
        />

        <label htmlFor="dropdown-selected-bg">Dropdown Selected:</label>
        <input
          id="dropdown-selected-bg"
          type="color"
          value={draft.dropdownSelectedBg}
          onChange={(event) =>
            handleFieldChange("dropdownSelectedBg", event.target.value)
          }
          title="theme.dropdown.option.selectedBgColor"
        />

        <label htmlFor="banner-bg">Banner:</label>
        <input
          id="banner-bg"
          type="color"
          value={draft.bannerBg}
          onChange={(event) => handleFieldChange("bannerBg", event.target.value)}
          title="theme.banner.bgColor"
        />

        <button id="connect-btn" onClick={() => void connect()} disabled={isConnecting}>
          {isConnecting ? "Connecting..." : "Connect"}
        </button>
      </div>

      <div id="status" className={status.tone === "idle" ? "" : status.tone}>
        {status.message}
      </div>

      <div className="demo-notes">
        <div>
          <strong>Bounded embed:</strong> set <code>scope.context</code> to scope the UI,
          and keep <code>scope.locked</code> on to prevent edits outside that slice.
        </div>
        <div>
          <strong>Theme:</strong> these controls map to nested{" "}
          <code>config.theme.colors</code>, <code>theme.button.primary</code>,{" "}
          <code>theme.icon</code>, <code>theme.search</code>, <code>theme.dropdown</code>,
          and <code>theme.banner</code> values.
        </div>
        <div>
          <strong>API:</strong> the UI uses its own REST client; <code>apiBaseUrl</code>{" "}
          decides where requests go.
        </div>
        <div>
          <strong>Override conditions:</strong> enable{" "}
          <code>Override Context Editing</code>
          to add context conditions in the create override modal. Leave the scoped filter
          empty if you want free-form context entry.
        </div>
      </div>

      <div id="app-root">
        {config ? (
          <SuperpositionUIProvider config={config}>
            <AlertProvider>
              <SuperpositionAdmin />
            </AlertProvider>
          </SuperpositionUIProvider>
        ) : (
          <p style={{ color: "#9ca3af", textAlign: "center", paddingTop: 80 }}>
            Configure connection above and click <strong>Connect</strong>
          </p>
        )}
      </div>
    </>
  );
}

const rootElement = document.getElementById("demo-root");

if (!rootElement) {
  throw new Error("Missing demo root element");
}

ReactDOM.createRoot(rootElement).render(
  <React.StrictMode>
    <DemoErrorBoundary>
      <DemoApp />
    </DemoErrorBoundary>
  </React.StrictMode>,
);
