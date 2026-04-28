import React, { useEffect, useRef } from "react";

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
  zIndex: 10000,
  padding: 24,
};

const dialogStyle: React.CSSProperties = {
  background: "var(--sp-color-panel)",
  borderRadius: "var(--sp-card-radius)",
  border: "1px solid var(--sp-color-border)",
  padding: 0,
  minWidth: 400,
  maxWidth: 600,
  maxHeight: "80vh",
  width: "min(620px, calc(100vw - 48px))",
  display: "flex",
  flexDirection: "column",
  boxShadow: "var(--sp-shadow-md)",
};

export function Modal({ open, onClose, title, children, footer }: ModalProps) {
  const ref = useRef<HTMLDivElement>(null);

  useEffect(() => {
    if (!open) return;
    const handleKeyDown = (e: KeyboardEvent) => {
      if (e.key === "Escape") onClose();
    };
    document.addEventListener("keydown", handleKeyDown);
    return () => document.removeEventListener("keydown", handleKeyDown);
  }, [open, onClose]);

  if (!open) return null;

  return (
    <div style={overlayStyle} onClick={onClose}>
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
            padding: "18px 22px",
            borderBottom: "1px solid var(--sp-color-border)",
            display: "flex",
            justifyContent: "space-between",
            alignItems: "center",
          }}
        >
          <h3
            style={{
              margin: 0,
              fontSize: 17,
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
              fontSize: 20,
              padding: 0,
              lineHeight: 1,
              color: "var(--sp-color-muted)",
            }}
            aria-label="Close"
          >
            ×
          </button>
        </div>
        <div style={{ padding: 22, overflowY: "auto", flex: 1 }}>{children}</div>
        {footer && (
          <div
            style={{
              padding: "14px 22px",
              borderTop: "1px solid var(--sp-color-border)",
              display: "flex",
              justifyContent: "flex-end",
              gap: 8,
            }}
          >
            {footer}
          </div>
        )}
      </div>
    </div>
  );
}
