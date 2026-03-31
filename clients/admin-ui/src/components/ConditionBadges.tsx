import React from "react";
import type { Condition } from "../types";

export interface ConditionBadgesProps {
  condition: Condition;
  lockedKeys?: string[];
}

const badgeStyle: React.CSSProperties = {
  display: "inline-flex",
  alignItems: "center",
  padding: "2px 8px",
  borderRadius: 12,
  fontSize: 12,
  fontWeight: 500,
  background: "#e0e7ff",
  color: "#3730a3",
  marginRight: 4,
  marginBottom: 4,
};

const lockedBadgeStyle: React.CSSProperties = {
  ...badgeStyle,
  background: "#fef3c7",
  color: "#92400e",
};

export function ConditionBadges({ condition, lockedKeys = [] }: ConditionBadgesProps) {
  const entries = Object.entries(condition);
  if (entries.length === 0) {
    return <span style={{ color: "#9ca3af", fontSize: 12 }}>No conditions</span>;
  }

  return (
    <div style={{ display: "flex", flexWrap: "wrap" }}>
      {entries.map(([key, value]) => {
        const isLocked = lockedKeys.includes(key);
        return (
          <span key={key} style={isLocked ? lockedBadgeStyle : badgeStyle}>
            {key} = {JSON.stringify(value)}
            {isLocked && " 🔒"}
          </span>
        );
      })}
    </div>
  );
}
