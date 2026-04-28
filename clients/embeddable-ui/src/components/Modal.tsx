import React, { useEffect, useRef } from "react";
import { createPortal } from "react-dom";
import { useOptionalSuperposition } from "../providers/SuperpositionUIProvider";

export interface ModalProps {
  open: boolean;
  onClose: () => void;
  title: string;
  children: React.ReactNode;
  footer?: React.ReactNode;
}

const overlayStyle: React.CSSProperties = {
  position: "fixed",
  inset: 0,
  background: "var(--sp-color-overlay)",
  display: "flex",
  alignItems: "center",
  justifyContent: "center",
  padding: "var(--sp-space-lg)",
};

const dialogStyle: React.CSSProperties = {
  background: "var(--sp-color-panel)",
  borderRadius: "var(--sp-card-radius)",
  border: "1px solid var(--sp-color-border)",
  padding: 0,
  minWidth: "var(--sp-modal-min-width)",
  maxWidth: "var(--sp-modal-max-width)",
  maxHeight: "var(--sp-modal-max-height)",
  width: "var(--sp-modal-width)",
  display: "flex",
  flexDirection: "column",
  boxShadow: "var(--sp-shadow-md)",
};

export function Modal({ open, onClose, title, children, footer }: ModalProps) {
  const ref = useRef<HTMLDivElement>(null);
  const context = useOptionalSuperposition();
  const ui = context?.config.ui;

  useEffect(() => {
    if (!open) return;
    const handleKeyDown = (e: KeyboardEvent) => {
      if (e.key === "Escape") onClose();
    };
    document.addEventListener("keydown", handleKeyDown);
    return () => document.removeEventListener("keydown", handleKeyDown);
  }, [open, onClose]);

  if (!open) return null;

  if (ui?.renderModal) {
    return (
      <>
        {ui.renderModal({
          open,
          onClose,
          title,
          children,
          footer,
        })}
      </>
    );
  }

  const overlay = (
    <div style={{ ...overlayStyle, zIndex: ui?.modalZIndex ?? 10000 }} onClick={onClose}>
      <div
        ref={ref}
        style={dialogStyle}
        role="dialog"
        aria-modal="true"
        aria-label={title}
        onClick={(e) => e.stopPropagation()}
      >
        <div
          style={{
            padding: "var(--sp-space-md) var(--sp-space-lg)",
            borderBottom: "1px solid var(--sp-color-border)",
            display: "flex",
            justifyContent: "space-between",
            alignItems: "center",
          }}
        >
          <h3
            style={{
              margin: 0,
              fontSize: "1.15rem",
              fontWeight: 700,
              color: "var(--sp-color-text)",
            }}
          >
            {title}
          </h3>
          <button
            onClick={onClose}
            style={{
              background: "none",
              border: "none",
              cursor: "pointer",
              fontSize: "1.4rem",
              padding: 0,
              lineHeight: 1,
              color: "var(--sp-icon-color)",
            }}
            aria-label="Close"
          >
            ×
          </button>
        </div>
        <div style={{ padding: "var(--sp-space-lg)", overflowY: "auto", flex: 1 }}>
          {children}
        </div>
        {footer && (
          <div
            style={{
              padding: "var(--sp-space-md) var(--sp-space-lg)",
              borderTop: "1px solid var(--sp-color-border)",
              display: "flex",
              justifyContent: "flex-end",
              gap: "var(--sp-space-sm)",
            }}
          >
            {footer}
          </div>
        )}
      </div>
    </div>
  );

  const portalTarget =
    typeof ui?.portalContainer === "function"
      ? ui.portalContainer()
      : typeof ui?.portalContainer === "string"
        ? document.querySelector(ui.portalContainer)
        : ui?.portalContainer;

  return portalTarget ? createPortal(overlay, portalTarget) : overlay;
}
