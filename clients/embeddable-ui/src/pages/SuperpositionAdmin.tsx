import React, { useState } from "react";
import { BoundaryFilterControl } from "../components/BoundaryFilterControl";
import { useSuperposition } from "../providers/SuperpositionProvider";
import {
  SUPERPOSITION_FEATURE_LABELS,
  SUPERPOSITION_FEATURES,
  type SuperpositionFeature,
} from "../types";
import { AuditLog } from "./AuditLog";
import { ConfigManager } from "./ConfigManager";
import { DimensionManager } from "./DimensionManager";
import { ExperimentManager } from "./ExperimentManager";
import { OverrideManager } from "./OverrideManager";
import { ResolveView } from "./ResolveView";

type Tab = SuperpositionFeature;

const allTabs: { id: Tab; label: string }[] = SUPERPOSITION_FEATURES.map((id) => ({
  id,
  label: SUPERPOSITION_FEATURE_LABELS[id],
}));

const tabComponents: Record<Tab, React.FC> = {
  config: ConfigManager,
  overrides: OverrideManager,
  dimensions: DimensionManager,
  experiments: ExperimentManager,
  resolve: ResolveView,
  audit: AuditLog,
};

export interface SuperpositionAdminProps {
  defaultFeature?: Tab;
  defaultTab?: Tab;
}

export function SuperpositionAdmin({
  defaultFeature,
  defaultTab = "config",
}: SuperpositionAdminProps) {
  const { config } = useSuperposition();
  const features = config.features;
  const routing = "routing" in config ? config.routing : undefined;

  const visibleTabs = features ? allTabs.filter((t) => features.includes(t.id)) : allTabs;

  const initialTab = routing?.initialFeature ?? defaultFeature ?? defaultTab;

  const [activeTab, setActiveTab] = useState<Tab>(
    visibleTabs.find((t) => t.id === initialTab)?.id ?? visibleTabs[0]?.id ?? "config",
  );

  const selectedTab =
    routing?.mode === "external"
      ? (visibleTabs.find((t) => t.id === routing.currentFeature)?.id ?? activeTab)
      : activeTab;

  const ActiveComponent = tabComponents[selectedTab];

  const handleTabChange = (tab: Tab) => {
    if (routing?.mode !== "external") {
      setActiveTab(tab);
    }

    routing?.onNavigate?.(tab);
  };

  return (
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
      <div style={{ display: "grid", gap: 20 }}>
        <div
          style={{
            display: "flex",
            justifyContent: "space-between",
            alignItems: "center",
            gap: 8,
            flexWrap: "wrap",
          }}
        >
          <BoundaryFilterControl />
          <div
            style={{
              display: "flex",
              gap: 8,
              flexWrap: "wrap",
              justifyContent: "flex-end",
            }}
          >
            {[`Org ${config.orgId}`, `Workspace ${config.workspace}`].map((item) => (
              <div
                key={item}
                style={{
                  padding: "8px 12px",
                  borderRadius: "var(--sp-pill-radius)",
                  background: "var(--sp-color-primary-soft)",
                  border:
                    "1px solid color-mix(in oklab, var(--sp-color-primary) 16%, var(--sp-color-border))",
                  color: "var(--sp-color-text)",
                  fontSize: 12,
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
            gap: 16,
            flexWrap: "wrap",
            padding: 8,
            background: "var(--sp-color-panel)",
            borderRadius: "var(--sp-card-radius)",
            border: "1px solid var(--sp-color-border)",
          }}
        >
          <div style={{ display: "flex", gap: 8, flexWrap: "wrap" }}>
            {visibleTabs.map((tab) =>
              (() => {
                const isActive = selectedTab === tab.id;
                const sharedStyle = {
                  padding: "10px 16px",
                  border: isActive
                    ? "1px solid color-mix(in oklab, var(--sp-color-primary) 22%, var(--sp-color-border))"
                    : "1px solid transparent",
                  borderRadius: "var(--sp-pill-radius)",
                  background: isActive ? "var(--sp-color-primary-soft)" : "transparent",
                  color: isActive ? "var(--sp-color-text)" : "var(--sp-color-muted)",
                  fontWeight: isActive ? 700 : 600,
                  cursor: "pointer",
                  fontSize: 13,
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
            minHeight: 620,
            overflow: "auto",
          }}
        >
          <ActiveComponent />
        </div>
      </div>
    </div>
  );
}
