import React, { useState } from "react";
import { useSuperposition } from "../providers/SuperpositionProvider";
import { ConfigManager } from "./ConfigManager";
import { DimensionManager } from "./DimensionManager";
import { OverrideManager } from "./OverrideManager";
import { ExperimentManager } from "./ExperimentManager";
import { ResolveView } from "./ResolveView";
import { AuditLog } from "./AuditLog";

type Tab = "config" | "overrides" | "dimensions" | "experiments" | "resolve" | "audit";

const allTabs: { id: Tab; label: string }[] = [
  { id: "config", label: "Configs" },
  { id: "overrides", label: "Overrides" },
  { id: "dimensions", label: "Dimensions" },
  { id: "experiments", label: "Experiments" },
  { id: "resolve", label: "Resolve" },
  { id: "audit", label: "Audit Log" },
];

const tabComponents: Record<Tab, React.FC> = {
  config: ConfigManager,
  overrides: OverrideManager,
  dimensions: DimensionManager,
  experiments: ExperimentManager,
  resolve: ResolveView,
  audit: AuditLog,
};

export interface SuperpositionAdminProps {
  defaultTab?: Tab;
}

export function SuperpositionAdmin({ defaultTab = "config" }: SuperpositionAdminProps) {
  const { config } = useSuperposition();
  const features = config.features;

  const visibleTabs = features
    ? allTabs.filter((t) => features.includes(t.id))
    : allTabs;

  const [activeTab, setActiveTab] = useState<Tab>(
    visibleTabs.find((t) => t.id === defaultTab)?.id ?? visibleTabs[0]?.id ?? "config",
  );

  const ActiveComponent = tabComponents[activeTab];

  return (
    <div style={{ fontFamily: '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif' }}>
      <div
        style={{
          display: "flex",
          borderBottom: "2px solid #e5e7eb",
          marginBottom: 20,
          gap: 0,
        }}
      >
        {visibleTabs.map((tab) => (
          <button
            key={tab.id}
            onClick={() => setActiveTab(tab.id)}
            style={{
              padding: "10px 20px",
              border: "none",
              borderBottom:
                activeTab === tab.id ? "2px solid #4f46e5" : "2px solid transparent",
              background: "none",
              color: activeTab === tab.id ? "#4f46e5" : "#6b7280",
              fontWeight: activeTab === tab.id ? 600 : 400,
              cursor: "pointer",
              fontSize: 14,
              marginBottom: -2,
            }}
          >
            {tab.label}
          </button>
        ))}
      </div>

      <ActiveComponent />
    </div>
  );
}
