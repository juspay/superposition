import React from "react";

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
  return (
    <div style={{ marginBottom: 16, opacity: disabled ? 0.6 : 1 }}>
      <label
        style={{
          display: "block",
          marginBottom: 4,
          fontSize: 13,
          fontWeight: 500,
          color: "#374151",
        }}
      >
        {label}
        {required && <span style={{ color: "#ef4444", marginLeft: 2 }}>*</span>}
      </label>
      {children}
      {error && (
        <p style={{ margin: "4px 0 0", fontSize: 12, color: "#ef4444" }}>{error}</p>
      )}
    </div>
  );
}

// ── Reusable input styles ──────────────────────────────────────────

export const inputStyle: React.CSSProperties = {
  width: "100%",
  padding: "8px 10px",
  border: "1px solid #d1d5db",
  borderRadius: 4,
  fontSize: 14,
  outline: "none",
  boxSizing: "border-box",
};

export const buttonPrimary: React.CSSProperties = {
  padding: "8px 16px",
  background: "#4f46e5",
  color: "#fff",
  border: "none",
  borderRadius: 4,
  fontSize: 14,
  cursor: "pointer",
  fontWeight: 500,
};

export const buttonSecondary: React.CSSProperties = {
  padding: "8px 16px",
  background: "#fff",
  color: "#374151",
  border: "1px solid #d1d5db",
  borderRadius: 4,
  fontSize: 14,
  cursor: "pointer",
};

export const buttonDanger: React.CSSProperties = {
  padding: "8px 16px",
  background: "#ef4444",
  color: "#fff",
  border: "none",
  borderRadius: 4,
  fontSize: 14,
  cursor: "pointer",
  fontWeight: 500,
};
