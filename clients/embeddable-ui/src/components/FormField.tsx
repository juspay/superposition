import React, { cloneElement, isValidElement, useId } from "react";

export interface FormFieldProps {
  label: string;
  required?: boolean;
  error?: string;
  children: React.ReactNode;
  disabled?: boolean;
}

export function FormField({
  label,
  required,
  error,
  children,
  disabled,
}: FormFieldProps) {
  const fieldId = useId();
  const messageId = `${fieldId}-error`;

  const control = isValidElement(children)
    ? cloneElement(children, {
        id: fieldId,
        disabled,
        "aria-invalid": error ? true : undefined,
        "aria-describedby": error ? messageId : undefined,
      })
    : children;

  return (
    <div style={{ marginBottom: 16, opacity: disabled ? 0.6 : 1 }}>
      <label
        htmlFor={fieldId}
        style={{
          display: "block",
          marginBottom: 4,
          fontSize: 13,
          fontWeight: 500,
          color: "var(--sp-color-text)",
        }}
      >
        {label}
        {required && (
          <span style={{ color: "var(--sp-feedback-danger-text)", marginLeft: 2 }}>
            *
          </span>
        )}
      </label>
      {control}
      {error && (
        <p
          id={messageId}
          style={{
            margin: "4px 0 0",
            fontSize: 12,
            color: "var(--sp-feedback-danger-text)",
          }}
        >
          {error}
        </p>
      )}
    </div>
  );
}

// ── Reusable input styles ──────────────────────────────────────────

export const inputStyle: React.CSSProperties = {
  width: "100%",
  padding: "10px 12px",
  border: "1px solid var(--sp-control-border)",
  background: "var(--sp-control-bg)",
  color: "var(--sp-control-text)",
  borderRadius: "var(--sp-control-radius)",
  fontSize: 14,
  outline: "none",
  boxSizing: "border-box",
  transition: "border-color 180ms ease, box-shadow 180ms ease, background 180ms ease",
};

export const buttonPrimary: React.CSSProperties = {
  padding: "10px 16px",
  background: "var(--sp-button-primary-bg)",
  color: "var(--sp-button-primary-text)",
  border: "1px solid var(--sp-button-primary-border)",
  borderRadius: "var(--sp-control-radius)",
  fontSize: 14,
  cursor: "pointer",
  fontWeight: 700,
  boxShadow: "var(--sp-button-primary-shadow)",
};

export const buttonSecondary: React.CSSProperties = {
  padding: "10px 16px",
  background: "var(--sp-button-secondary-bg)",
  color: "var(--sp-button-secondary-text)",
  border: "1px solid var(--sp-button-secondary-border)",
  borderRadius: "var(--sp-control-radius)",
  fontSize: 14,
  cursor: "pointer",
  fontWeight: 600,
};

export const buttonDanger: React.CSSProperties = {
  padding: "8px 12px",
  background: "var(--sp-button-danger-bg)",
  color: "var(--sp-button-danger-text)",
  border: "1px solid var(--sp-button-danger-border)",
  borderRadius: "var(--sp-inline-radius)",
  fontSize: 13,
  cursor: "pointer",
  fontWeight: 700,
};
