import { defineCustomElements } from "../src/browser";
import { SUPERPOSITION_FEATURE_LABELS, SUPERPOSITION_FEATURES } from "../src/types";
import type { SuperpositionFeature, SuperpositionThemeMode } from "../src/types";

interface DemoDraftState {
  host: string;
  orgId: string;
  workspace: string;
  features: string;
  scopeJson: string;
  scopeLocked: boolean;
  themeMode: SuperpositionThemeMode;
  colorPrimary: string;
  colorBg: string;
  colorPanel: string;
}

const STORAGE_KEY = "sp-embeddable-custom-elements-demo";

const DEFAULT_DRAFT: DemoDraftState = {
  host: "/api",
  orgId: "localorg",
  workspace: "dev",
  features: "config,overrides,resolve",
  scopeJson: '{"region":"us-east-1"}',
  scopeLocked: true,
  themeMode: "system",
  colorPrimary: "#0f766e",
  colorBg: "#ecfeff",
  colorPanel: "#ffffff",
};

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

function parseOptionalJsonObject(value: string): Record<string, unknown> | undefined {
  const trimmed = value.trim();
  if (!trimmed) return undefined;

  const parsed = JSON.parse(trimmed) as unknown;
  if (!parsed || Array.isArray(parsed) || typeof parsed !== "object") {
    throw new Error("Scoped filter must be a JSON object.");
  }

  return parsed as Record<string, unknown>;
}

function readPersistedState(): DemoDraftState {
  const raw = window.localStorage.getItem(STORAGE_KEY);
  if (!raw) return DEFAULT_DRAFT;

  try {
    return { ...DEFAULT_DRAFT, ...(JSON.parse(raw) as Partial<DemoDraftState>) };
  } catch {
    window.localStorage.removeItem(STORAGE_KEY);
    return DEFAULT_DRAFT;
  }
}

function persistState(state: DemoDraftState) {
  window.localStorage.setItem(STORAGE_KEY, JSON.stringify(state));
}

function buildConfig(state: DemoDraftState) {
  const host = state.host.replace(/\/+$/, "");
  const features = parseFeatureList(state.features);

  return {
    apiBaseUrl: host,
    orgId: state.orgId,
    workspace: state.workspace,
    features,
    scope: {
      context: parseOptionalJsonObject(state.scopeJson),
      locked: state.scopeLocked,
    },
    theme: {
      mode: state.themeMode,
      colorPrimary: state.colorPrimary || undefined,
      colorBg: state.colorBg || undefined,
      colorPanel: state.colorPanel || undefined,
    },
  };
}

defineCustomElements();

const form = document.getElementById("demo-form") as HTMLFormElement | null;
const status = document.getElementById("status") as HTMLDivElement | null;
const embedded = document.getElementById("embedded-admin") as HTMLElement | null;
const apiChip = document.getElementById("api-chip") as HTMLSpanElement | null;
const featurePicker = document.getElementById("features") as HTMLDivElement | null;
const featureValue = document.getElementById("featuresValue") as HTMLInputElement | null;

if (!form || !status || !embedded || !apiChip || !featurePicker || !featureValue) {
  throw new Error("Missing custom elements demo nodes");
}

const demoForm = form;
const statusNode = status;
const embeddedNode = embedded;
const apiChipNode = apiChip;
const featurePickerNode = featurePicker;
const featureValueNode = featureValue;

let state = readPersistedState();

function syncFeaturePicker(value: string) {
  const selectedFeatures = parseFeatureList(value);
  const allFeaturesSelected = !selectedFeatures;
  featureValueNode.value = value;

  for (const checkbox of featurePickerNode.querySelectorAll<HTMLInputElement>(
    "input[data-feature]",
  )) {
    const feature = checkbox.dataset.feature;
    checkbox.checked =
      allFeaturesSelected ||
      Boolean(feature && selectedFeatures?.includes(feature as SuperpositionFeature));
  }

  const allCheckbox = featurePickerNode.querySelector<HTMLInputElement>(
    'input[data-feature-all="true"]',
  );
  if (allCheckbox) {
    allCheckbox.checked = allFeaturesSelected;
  }
}

function setFeatureValue(value: string) {
  state = { ...state, features: value };
  syncFeaturePicker(value);
}

function renderFeaturePicker() {
  featurePickerNode.replaceChildren();

  const allLabel = document.createElement("label");
  const allCheckbox = document.createElement("input");
  allCheckbox.type = "checkbox";
  allCheckbox.dataset.featureAll = "true";
  allCheckbox.addEventListener("change", () => {
    setFeatureValue(allCheckbox.checked ? "" : "config");
  });
  allLabel.append(allCheckbox, "All");
  featurePickerNode.appendChild(allLabel);

  for (const feature of SUPERPOSITION_FEATURES) {
    const label = document.createElement("label");
    const checkbox = document.createElement("input");
    checkbox.type = "checkbox";
    checkbox.dataset.feature = feature;
    checkbox.addEventListener("change", () => {
      const selectedFeatures = parseFeatureList(featureValueNode.value) ?? [
        ...SUPERPOSITION_FEATURES,
      ];
      const next = checkbox.checked
        ? Array.from(new Set([...selectedFeatures, feature]))
        : selectedFeatures.filter((item) => item !== feature);
      setFeatureValue(
        next.length === SUPERPOSITION_FEATURES.length
          ? ""
          : serializeFeatureList(next.length > 0 ? next : [feature]),
      );
    });
    label.append(checkbox, SUPERPOSITION_FEATURE_LABELS[feature]);
    featurePickerNode.appendChild(label);
  }
}

renderFeaturePicker();
syncFeaturePicker(state.features);

for (const [key, value] of Object.entries(state)) {
  const field = demoForm.elements.namedItem(key);
  if (!(field instanceof HTMLInputElement || field instanceof HTMLSelectElement))
    continue;

  if (field instanceof HTMLInputElement && field.type === "checkbox") {
    field.checked = Boolean(value);
  } else {
    field.value = String(value);
  }
}

function setStatus(kind: "idle" | "info" | "success" | "error", message: string) {
  statusNode.className = kind === "idle" ? "" : kind;
  statusNode.textContent = message;
}

async function mount() {
  const formData = new FormData(demoForm);
  state = {
    host: String(formData.get("host") ?? ""),
    orgId: String(formData.get("orgId") ?? ""),
    workspace: String(formData.get("workspace") ?? ""),
    features: String(formData.get("features") ?? ""),
    scopeJson: String(formData.get("scopeJson") ?? ""),
    scopeLocked: formData.get("scopeLocked") === "on",
    themeMode: String(formData.get("themeMode") ?? "system") as SuperpositionThemeMode,
    colorPrimary: String(formData.get("colorPrimary") ?? ""),
    colorBg: String(formData.get("colorBg") ?? ""),
    colorPanel: String(formData.get("colorPanel") ?? ""),
  };

  persistState(state);
  apiChipNode.textContent = `API: ${state.host.trim().startsWith("/") ? "host proxy" : "direct API"}`;

  try {
    parseOptionalJsonObject(state.scopeJson);
  } catch (error) {
    setStatus("error", error instanceof Error ? error.message : "Invalid scope JSON.");
    return;
  }

  const host = state.host.replace(/\/+$/, "");
  setStatus("info", `Connecting to ${host} ...`);

  try {
    const resp = await fetch(host + "/health");
    if (!resp.ok) throw new Error(`HTTP ${resp.status}`);
    embeddedNode.setAttribute("config", JSON.stringify(buildConfig(state)));
    setStatus("success", `Mounted <superposition-admin> from ${host}`);
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
    setStatus("error", `Cannot reach ${host}/health — ${message}`);
  }
}

demoForm.addEventListener("submit", (event) => {
  event.preventDefault();
  void mount();
});

void mount();
