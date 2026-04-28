import React, { createContext, useCallback, useContext, useState } from "react";
import { createPortal } from "react-dom";
import type { ConfirmInput, SuperpositionUiAdapters } from "../types";
import { confirmAction as resolveConfirmAction } from "../utils/ui-adapters";
import { useOptionalSuperposition } from "./SuperpositionUIProvider";

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
  confirmAction: (input: ConfirmInput) => Promise<boolean>;
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

function resolvePortalTarget(ui?: SuperpositionUiAdapters): Element | null {
  if (!ui?.portalContainer || typeof document === "undefined") {
    return null;
  }

  if (typeof ui.portalContainer === "function") {
    return ui.portalContainer();
  }

  if (typeof ui.portalContainer === "string") {
    return document.querySelector(ui.portalContainer);
  }

  return ui.portalContainer;
}

export function AlertBar({
  zIndex = 9999,
  portalContainer,
}: {
  zIndex?: number;
  portalContainer?: Element | null;
} = {}) {
  const { alerts, removeAlert } = useAlerts();

  if (alerts.length === 0) {
    return null;
  }

  const content = (
    <div
      style={{
        position: "fixed",
        top: "var(--sp-space-md)",
        right: "var(--sp-space-md)",
        zIndex,
        display: "flex",
        flexDirection: "column",
        gap: "var(--sp-space-xs)",
      }}
    >
      {alerts.map((alert) => {
        const palette: Record<AlertType, React.CSSProperties> = {
          success: {
            background: "var(--sp-toast-success-bg)",
            color: "var(--sp-toast-success-text)",
            border: "1px solid var(--sp-toast-success-border)",
          },
          error: {
            background: "var(--sp-toast-error-bg)",
            color: "var(--sp-toast-error-text)",
            border: "1px solid var(--sp-toast-error-border)",
          },
          warning: {
            background: "var(--sp-toast-warning-bg)",
            color: "var(--sp-toast-warning-text)",
            border: "1px solid var(--sp-toast-warning-border)",
          },
          info: {
            background: "var(--sp-toast-info-bg)",
            color: "var(--sp-toast-info-text)",
            border: "1px solid var(--sp-toast-info-border)",
          },
        };

        return (
          <div
            key={alert.id}
            style={{
              ...palette[alert.type],
              padding: "var(--sp-toast-padding)",
              borderRadius: "var(--sp-toast-radius)",
              display: "flex",
              justifyContent: "space-between",
              alignItems: "center",
              minWidth: "var(--sp-alert-min-width)",
              fontSize: "var(--sp-toast-font-size)",
              fontWeight: "var(--sp-toast-font-weight)",
              boxShadow: "var(--sp-toast-shadow)",
            }}
          >
            <span>{alert.message}</span>
            <button
              onClick={() => removeAlert(alert.id)}
              style={{
                background: "none",
                border: "none",
                cursor: "pointer",
                padding: "0 0 0 var(--sp-space-sm)",
                fontSize: "1.15rem",
                color: "currentColor",
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

  return portalContainer ? createPortal(content, portalContainer) : content;
}

export function AlertProvider({ children }: { children: React.ReactNode }) {
  const superposition = useOptionalSuperposition();
  const ui = superposition?.config.ui;
  const [alerts, setAlerts] = useState<Alert[]>([]);
  const [confirmRequest, setConfirmRequest] = useState<{
    input: Parameters<AlertContextValue["confirmAction"]>[0];
    resolve: (confirmed: boolean) => void;
  } | null>(null);

  const addAlert = useCallback(
    (type: AlertType, message: string) => {
      if (ui?.notify) {
        ui.notify({ tone: type, title: message });
        return;
      }

      const id = `alert-${++alertCounter}`;
      setAlerts((prev) => [...prev, { id, type, message }]);
      setTimeout(() => {
        setAlerts((prev) => prev.filter((a) => a.id !== id));
      }, 5000);
    },
    [ui],
  );

  const removeAlert = useCallback((id: string) => {
    setAlerts((prev) => prev.filter((a) => a.id !== id));
  }, []);

  const confirmAction = useCallback<AlertContextValue["confirmAction"]>(
    async (input) =>
      resolveConfirmAction(
        ui,
        input,
        () =>
          new Promise<boolean>((resolve) => {
            setConfirmRequest({ input, resolve });
          }),
      ),
    [ui],
  );

  const closeConfirm = useCallback((confirmed: boolean) => {
    setConfirmRequest((current) => {
      current?.resolve(confirmed);
      return null;
    });
  }, []);

  const portalTarget = resolvePortalTarget(ui);
  const isDestructiveConfirm = confirmRequest?.input.variant === "destructive";
  const confirmFooter = confirmRequest ? (
    <>
      <button
        type="button"
        onClick={() => closeConfirm(false)}
        style={{
          padding: "var(--sp-space-sm) var(--sp-space-md)",
          background: "var(--sp-button-secondary-bg)",
          color: "var(--sp-button-secondary-text)",
          border: "1px solid var(--sp-button-secondary-border)",
          borderRadius: "var(--sp-control-radius)",
          font: "inherit",
          fontWeight: 600,
          cursor: "pointer",
        }}
      >
        {confirmRequest.input.cancelLabel ?? "Cancel"}
      </button>
      <button
        type="button"
        onClick={() => closeConfirm(true)}
        style={{
          padding: "var(--sp-space-sm) var(--sp-space-md)",
          background: isDestructiveConfirm
            ? "var(--sp-feedback-danger-bg)"
            : "var(--sp-button-primary-bg)",
          color: isDestructiveConfirm
            ? "var(--sp-feedback-danger-text)"
            : "var(--sp-button-primary-text)",
          border: isDestructiveConfirm
            ? "1px solid var(--sp-feedback-danger-border)"
            : "1px solid var(--sp-button-primary-border)",
          borderRadius: "var(--sp-control-radius)",
          font: "inherit",
          fontWeight: 700,
          cursor: "pointer",
        }}
      >
        {confirmRequest.input.confirmLabel ?? "Confirm"}
      </button>
    </>
  ) : null;

  const confirmContent = confirmRequest ? (
    <div style={{ color: "var(--sp-color-muted)", lineHeight: 1.5 }}>
      {confirmRequest.input.description}
    </div>
  ) : null;

  return (
    <AlertContext.Provider value={{ alerts, addAlert, removeAlert, confirmAction }}>
      {children}
      {!ui?.notify && (
        <AlertBar zIndex={ui?.alertZIndex ?? 9999} portalContainer={portalTarget} />
      )}
      {confirmRequest &&
        ui?.renderModal?.({
          open: true,
          onClose: () => closeConfirm(false),
          title: confirmRequest.input.title,
          children: confirmContent,
          footer: confirmFooter,
        })}
      {confirmRequest &&
        !ui?.renderModal &&
        createPortal(
          <div
            style={{
              position: "fixed",
              inset: 0,
              zIndex: ui?.modalZIndex ?? 10000,
              background: "var(--sp-color-overlay)",
              display: "flex",
              alignItems: "center",
              justifyContent: "center",
              padding: "var(--sp-space-lg)",
            }}
            onClick={() => closeConfirm(false)}
          >
            <div
              role="dialog"
              aria-modal="true"
              aria-label={confirmRequest.input.title}
              style={{
                width: "var(--sp-confirm-width)",
                background: "var(--sp-color-panel)",
                border: "1px solid var(--sp-color-border)",
                borderRadius: "var(--sp-card-radius)",
                boxShadow: "var(--sp-shadow-md)",
                padding: "var(--sp-space-lg)",
                display: "grid",
                gap: "var(--sp-space-md)",
              }}
              onClick={(event) => event.stopPropagation()}
            >
              <div style={{ fontSize: "1.05rem", fontWeight: 700 }}>
                {confirmRequest.input.title}
              </div>
              {confirmRequest.input.description && confirmContent}
              <div
                style={{
                  display: "flex",
                  justifyContent: "flex-end",
                  gap: "var(--sp-space-sm)",
                }}
              >
                {confirmFooter}
              </div>
            </div>
          </div>,
          portalTarget ?? document.body,
        )}
    </AlertContext.Provider>
  );
}
