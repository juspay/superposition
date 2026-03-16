import { useState } from "react";

export interface JsonViewerProps {
  data: unknown;
  collapsed?: boolean;
}

export function JsonViewer({ data, collapsed = true }: JsonViewerProps) {
  const [expanded, setExpanded] = useState(!collapsed);

  const formatted = JSON.stringify(data, null, 2);
  const preview = JSON.stringify(data);
  const isLong = preview.length > 60;

  if (!isLong) {
    return (
      <code style={{ fontSize: 13, fontFamily: "monospace", wordBreak: "break-all" }}>
        {preview}
      </code>
    );
  }

  return (
    <div>
      {expanded ? (
        <pre
          style={{
            fontSize: 13,
            fontFamily: "monospace",
            background: "#f3f4f6",
            padding: 8,
            borderRadius: 4,
            overflow: "auto",
            maxHeight: 300,
            margin: 0,
          }}
        >
          {formatted}
        </pre>
      ) : (
        <code style={{ fontSize: 13, fontFamily: "monospace" }}>
          {preview.slice(0, 57) + "..."}
        </code>
      )}
      <button
        onClick={() => setExpanded(!expanded)}
        style={{
          background: "none",
          border: "none",
          color: "#4f46e5",
          cursor: "pointer",
          fontSize: 12,
          padding: 0,
          marginLeft: 4,
        }}
      >
        {expanded ? "collapse" : "expand"}
      </button>
    </div>
  );
}
