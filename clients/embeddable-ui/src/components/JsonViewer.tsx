import { useState } from "react";

export interface JsonViewerProps {
  data: unknown;
  collapsed?: boolean;
}

export function JsonViewer({ data, collapsed = true }: JsonViewerProps) {
  const [expanded, setExpanded] = useState(!collapsed);

  const formatted = JSON.stringify(data, null, 2) ?? String(data);
  const preview = JSON.stringify(data) ?? String(data);
  const isLong = preview.length > 60;

  if (!isLong) {
    return (
      <code
        style={{
          fontSize: 12,
          fontFamily: "ui-monospace, SFMono-Regular, Menlo, monospace",
          wordBreak: "break-all",
          display: "inline-block",
          padding: "4px 8px",
          borderRadius: "var(--sp-pill-radius)",
          background: "var(--sp-color-surface-subtle)",
          border: "1px solid var(--sp-color-border)",
        }}
      >
        {preview}
      </code>
    );
  }

  return (
    <div>
      {expanded ? (
        <pre
          style={{
            fontSize: 12,
            fontFamily: "ui-monospace, SFMono-Regular, Menlo, monospace",
            background: "var(--sp-color-surface-subtle)",
            padding: 10,
            borderRadius: "var(--sp-inline-radius)",
            border: "1px solid var(--sp-color-border)",
            overflow: "auto",
            maxHeight: 300,
            margin: 0,
          }}
        >
          {formatted}
        </pre>
      ) : (
        <code
          style={{
            fontSize: 12,
            fontFamily: "ui-monospace, SFMono-Regular, Menlo, monospace",
            display: "inline-block",
            padding: "4px 8px",
            borderRadius: "var(--sp-pill-radius)",
            background: "var(--sp-color-surface-subtle)",
            border: "1px solid var(--sp-color-border)",
          }}
        >
          {preview.slice(0, 57) + "..."}
        </code>
      )}
      <button
        onClick={() => setExpanded(!expanded)}
        style={{
          background: "none",
          border: "none",
          color: "var(--sp-color-primary)",
          cursor: "pointer",
          fontSize: 12,
          padding: 0,
          marginLeft: 6,
          fontWeight: 700,
        }}
      >
        {expanded ? "collapse" : "expand"}
      </button>
    </div>
  );
}
