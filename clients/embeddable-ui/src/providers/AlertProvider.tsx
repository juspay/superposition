import React, { createContext, useCallback, useContext, useState } from "react";

export type AlertType = "success" | "error" | "warning" | "info";

export interface Alert {
  id: string;
  type: AlertType;
  message: string;
}

interface AlertContextValue {
  alerts: Alert[];
  addAlert: (type: AlertType, message: string) => void;
  removeAlert: (id: string) => void;
}

const AlertContext = createContext<AlertContextValue | null>(null);

export function useAlerts(): AlertContextValue {
  const ctx = useContext(AlertContext);
  if (!ctx) {
    throw new Error("useAlerts must be used within an <AlertProvider>");
  }
  return ctx;
}

let alertCounter = 0;

export function AlertBar() {
  const { alerts, removeAlert } = useAlerts();

  if (alerts.length === 0) {
    return null;
  }

  return (
    <div
      style={{
        position: "fixed",
        top: 16,
        right: 16,
        zIndex: 9999,
        display: "flex",
        flexDirection: "column",
        gap: 8,
      }}
    >
      {alerts.map((alert) => {
        const palette: Record<AlertType, React.CSSProperties> = {
          success: {
            background: "var(--sp-feedback-success-bg)",
            color: "var(--sp-feedback-success-text)",
            border: "1px solid var(--sp-feedback-success-border)",
          },
          error: {
            background: "var(--sp-feedback-danger-bg)",
            color: "var(--sp-feedback-danger-text)",
            border: "1px solid var(--sp-feedback-danger-border)",
          },
          warning: {
            background: "var(--sp-feedback-warning-bg)",
            color: "var(--sp-feedback-warning-text)",
            border: "1px solid var(--sp-feedback-warning-border)",
          },
          info: {
            background: "var(--sp-feedback-info-bg)",
            color: "var(--sp-feedback-info-text)",
            border: "1px solid var(--sp-feedback-info-border)",
          },
        };

        return (
          <div
            key={alert.id}
            style={{
              ...palette[alert.type],
              padding: "12px 16px",
              borderRadius: "var(--sp-inline-radius)",
              display: "flex",
              justifyContent: "space-between",
              alignItems: "center",
              minWidth: 300,
              fontSize: 14,
              boxShadow: "var(--sp-shadow-sm)",
            }}
          >
            <span>{alert.message}</span>
            <button
              onClick={() => removeAlert(alert.id)}
              style={{
                background: "none",
                border: "none",
                cursor: "pointer",
                padding: "0 0 0 12px",
                fontSize: 16,
              }}
              aria-label="Dismiss"
            >
              ×
            </button>
          </div>
        );
      })}
    </div>
  );
}

export function AlertProvider({ children }: { children: React.ReactNode }) {
  const [alerts, setAlerts] = useState<Alert[]>([]);

  const addAlert = useCallback((type: AlertType, message: string) => {
    const id = `alert-${++alertCounter}`;
    setAlerts((prev) => [...prev, { id, type, message }]);
    setTimeout(() => {
      setAlerts((prev) => prev.filter((a) => a.id !== id));
    }, 5000);
  }, []);

  const removeAlert = useCallback((id: string) => {
    setAlerts((prev) => prev.filter((a) => a.id !== id));
  }, []);

  return (
    <AlertContext.Provider value={{ alerts, addAlert, removeAlert }}>
      {children}
      <AlertBar />
    </AlertContext.Provider>
  );
}
