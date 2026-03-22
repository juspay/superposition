import React from "react";
import { useAlerts } from "../providers/AlertProvider";
import type { AlertType } from "../providers/AlertProvider";

const alertStyles: Record<AlertType, React.CSSProperties> = {
  success: { background: "#d1fae5", color: "#065f46", border: "1px solid #6ee7b7" },
  error: { background: "#fee2e2", color: "#991b1b", border: "1px solid #fca5a5" },
  warning: { background: "#fef3c7", color: "#92400e", border: "1px solid #fcd34d" },
  info: { background: "#dbeafe", color: "#1e40af", border: "1px solid #93c5fd" },
};

export function AlertBar() {
  const { alerts, removeAlert } = useAlerts();

  if (alerts.length === 0) return null;

  return (
    <div style={{ position: "fixed", top: 16, right: 16, zIndex: 9999, display: "flex", flexDirection: "column", gap: 8 }}>
      {alerts.map((alert) => (
        <div
          key={alert.id}
          style={{
            ...alertStyles[alert.type],
            padding: "12px 16px",
            borderRadius: 6,
            display: "flex",
            justifyContent: "space-between",
            alignItems: "center",
            minWidth: 300,
            fontSize: 14,
            boxShadow: "0 2px 8px rgba(0,0,0,0.1)",
          }}
        >
          <span>{alert.message}</span>
          <button
            onClick={() => removeAlert(alert.id)}
            style={{ background: "none", border: "none", cursor: "pointer", padding: "0 0 0 12px", fontSize: 16 }}
            aria-label="Dismiss"
          >
            ×
          </button>
        </div>
      ))}
    </div>
  );
}
