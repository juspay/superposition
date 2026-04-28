import React, { useState } from "react";
import { BoundaryFilterControl } from "../components/BoundaryFilterControl";
import { AlertProvider } from "../providers/AlertProvider";
import { useSuperposition } from "../providers/SuperpositionUIProvider";
import {
  SUPERPOSITION_FEATURE_LABELS,
  SUPERPOSITION_FEATURES,
  type SuperpositionFeature,
} from "../types";
import { ConfigManager } from "./ConfigManager";
import { DimensionManager } from "./DimensionManager";
import { getMessage } from "./FeatureGate";
import { OverrideManager } from "./OverrideManager";

type Tab = SuperpositionFeature;

const allTabs: { id: Tab; label: string }[] = SUPERPOSITION_FEATURES.map((id) => ({
  id,
  label: SUPERPOSITION_FEATURE_LABELS[id],
}));

const tabComponents: Record<Tab, React.FC> = {
  config: ConfigManager,
  overrides: OverrideManager,
  dimensions: DimensionManager,
};

export interface SuperpositionAdminProps {
  defaultFeature?: Tab;
  defaultTab?: Tab;
  allowConfigEditing?: boolean;
  allowDimensionEditing?: boolean;
}

export function SuperpositionAdmin({
  defaultFeature,
  defaultTab = "config",
  allowConfigEditing = false,
  allowDimensionEditing = false,
}: SuperpositionAdminProps) {
  const { config } = useSuperposition();
  const features = config.features;
  const routing = "routing" in config ? config.routing : undefined;
  const showBoundaryFilter = config.ui?.showBoundaryFilter ?? !config.strict;

  const visibleTabs = features ? allTabs.filter((t) => features.includes(t.id)) : allTabs;

  const initialTab = routing?.initialFeature ?? defaultFeature ?? defaultTab;

  const [activeTab, setActiveTab] = useState<Tab>(
    visibleTabs.find((t) => t.id === initialTab)?.id ?? visibleTabs[0]?.id ?? "config",
  );

  const selectedTab =
    routing?.mode === "external"
      ? (visibleTabs.find((t) => t.id === routing.currentFeature)?.id ??
        visibleTabs[0]?.id ??
        activeTab)
      : (visibleTabs.find((t) => t.id === activeTab)?.id ??
        visibleTabs[0]?.id ??
        activeTab);

  const ActiveComponent = tabComponents[selectedTab];

  const handleTabChange = (tab: Tab) => {
    if (routing?.mode !== "external") {
      setActiveTab(tab);
    }

    routing?.onNavigate?.(tab);
  };

  if (visibleTabs.length === 0) {
    return (
      <AlertProvider>
        <div
          role="status"
          style={{
            fontFamily: "inherit",
            color: "var(--sp-color-muted)",
            background: "var(--sp-color-bg)",
            border: "1px solid var(--sp-color-border)",
            borderRadius: "var(--sp-radius-lg)",
            padding: "var(--sp-space-lg)",
          }}
        >
          {getMessage(
            config,
            "admin.noFeatures",
            "No Superposition features are enabled for this embed.",
          )}
        </div>
      </AlertProvider>
    );
  }

  return (
    <AlertProvider>
      <div
        style={{
          fontFamily: "inherit",
          color: "var(--sp-color-text)",
          background: "var(--sp-color-bg)",
          border: "1px solid var(--sp-color-border)",
          borderRadius: "var(--sp-radius-lg)",
          padding: "var(--sp-space-lg)",
          boxShadow: "var(--sp-shadow-sm)",
        }}
      >
        <div style={{ display: "grid", gap: "var(--sp-space-lg)" }}>
          <div
            style={{
              display: "flex",
              justifyContent: "space-between",
              alignItems: "center",
              gap: "var(--sp-space-sm)",
              flexWrap: "wrap",
            }}
          >
            {showBoundaryFilter ? <BoundaryFilterControl /> : null}
            <div
              style={{
                display: "flex",
                gap: "var(--sp-space-sm)",
                flexWrap: "wrap",
                justifyContent: "flex-end",
              }}
            >
              {[`Org ${config.orgId}`, `Workspace ${config.workspace}`].map((item) => (
                <div
                  key={item}
                  style={{
                    padding: "var(--sp-space-xs) var(--sp-space-sm)",
                    borderRadius: "var(--sp-pill-radius)",
                    background: "var(--sp-color-primary-soft)",
                    border:
                      "1px solid color-mix(in oklab, var(--sp-color-primary) 16%, var(--sp-color-border))",
                    color: "var(--sp-color-text)",
                    fontSize: "0.86rem",
                    fontWeight: 600,
                  }}
                >
                  {item}
                </div>
              ))}
            </div>
          </div>

          <div
            style={{
              display: "flex",
              alignItems: "center",
              gap: "var(--sp-space-md)",
              flexWrap: "wrap",
              padding: "var(--sp-space-xs)",
              background: "var(--sp-color-panel)",
              borderRadius: "var(--sp-card-radius)",
              border: "1px solid var(--sp-color-border)",
            }}
          >
            <div style={{ display: "flex", gap: "var(--sp-space-sm)", flexWrap: "wrap" }}>
              {visibleTabs.map((tab) =>
                (() => {
                  const isActive = selectedTab === tab.id;
                  const sharedStyle = {
                    padding: "var(--sp-space-sm) var(--sp-space-md)",
                    border: isActive
                      ? "1px solid color-mix(in oklab, var(--sp-color-primary) 22%, var(--sp-color-border))"
                      : "1px solid transparent",
                    borderRadius: "var(--sp-pill-radius)",
                    background: isActive ? "var(--sp-color-primary-soft)" : "transparent",
                    color: isActive ? "var(--sp-color-text)" : "var(--sp-color-muted)",
                    fontWeight: isActive ? 700 : 600,
                    cursor: "pointer",
                    fontSize: "0.93rem",
                    textDecoration: "none",
                    transition:
                      "background 180ms ease, border-color 180ms ease, color 180ms ease",
                  } as const;

                  const href = routing?.getFeatureHref?.(tab.id);

                  if (href) {
                    return (
                      <a
                        key={tab.id}
                        href={href}
                        onClick={(event) => {
                          event.preventDefault();
                          handleTabChange(tab.id);
                        }}
                        style={sharedStyle}
                      >
                        {tab.label}
                      </a>
                    );
                  }

                  return (
                    <button
                      key={tab.id}
                      onClick={() => handleTabChange(tab.id)}
                      style={sharedStyle}
                    >
                      {tab.label}
                    </button>
                  );
                })(),
              )}
            </div>
          </div>

          <div
            style={{
              background: "var(--sp-color-panel)",
              border: "1px solid var(--sp-color-border)",
              borderRadius: "var(--sp-radius-lg)",
              padding: "var(--sp-space-lg)",
              minHeight: "var(--sp-admin-content-min-height)",
              overflow: "auto",
            }}
          >
            {selectedTab === "config" ? (
              <ConfigManager editable={allowConfigEditing} />
            ) : selectedTab === "dimensions" ? (
              <DimensionManager editable={allowDimensionEditing} />
            ) : (
              <ActiveComponent />
            )}
          </div>
        </div>
      </div>
    </AlertProvider>
  );
}
