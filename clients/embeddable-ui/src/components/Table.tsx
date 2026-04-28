import React from "react";

export interface Column<T> {
  key: string;
  header: string;
  render?: (row: T) => React.ReactNode;
  width?: string;
}

export interface TableProps<T> {
  columns: Column<T>[];
  data: T[];
  keyExtractor: (row: T) => string;
  onRowClick?: (row: T) => void;
  emptyMessage?: string;
  loading?: boolean;
}

const tableStyle: React.CSSProperties = {
  width: "100%",
  borderCollapse: "collapse",
  fontSize: 14,
  background: "var(--sp-color-panel)",
};

const thStyle: React.CSSProperties = {
  textAlign: "left",
  padding: "14px 16px",
  borderBottom: "1px solid var(--sp-color-border)",
  fontWeight: 700,
  color: "var(--sp-color-muted)",
  background: "var(--sp-color-surface-muted)",
  fontSize: 12,
  letterSpacing: "0.04em",
  textTransform: "uppercase",
};

const tdStyle: React.CSSProperties = {
  padding: "16px",
  borderBottom: "1px solid color-mix(in oklab, var(--sp-color-border) 75%, transparent)",
  color: "var(--sp-color-text)",
  verticalAlign: "top",
};

export function Table<T>({
  columns,
  data,
  keyExtractor,
  onRowClick,
  emptyMessage = "No data",
  loading = false,
}: TableProps<T>) {
  if (loading) {
    return (
      <div
        style={{
          padding: 48,
          textAlign: "center",
          color: "var(--sp-color-muted)",
          border: "1px solid var(--sp-color-border)",
          borderRadius: "var(--sp-card-radius)",
          background: "var(--sp-color-panel)",
        }}
      >
        Loading...
      </div>
    );
  }

  return (
    <div
      style={{
        overflowX: "auto",
        border: "1px solid var(--sp-color-border)",
        borderRadius: "var(--sp-card-radius)",
        background: "var(--sp-color-panel)",
      }}
    >
      <table style={tableStyle}>
        <thead>
          <tr>
            {columns.map((col) => (
              <th key={col.key} style={{ ...thStyle, width: col.width }}>
                {col.header}
              </th>
            ))}
          </tr>
        </thead>
        <tbody>
          {data.length === 0 ? (
            <tr>
              <td
                colSpan={columns.length}
                style={{
                  ...tdStyle,
                  textAlign: "center",
                  color: "var(--sp-color-muted)",
                  padding: "44px 20px",
                  fontSize: 15,
                }}
              >
                {emptyMessage}
              </td>
            </tr>
          ) : (
            data.map((row) => (
              <tr
                key={keyExtractor(row)}
                onClick={() => onRowClick?.(row)}
                style={{
                  cursor: onRowClick ? "pointer" : "default",
                }}
              >
                {columns.map((col) => (
                  <td key={col.key} style={tdStyle}>
                    {col.render
                      ? col.render(row)
                      : String((row as Record<string, unknown>)[col.key] ?? "")}
                  </td>
                ))}
              </tr>
            ))
          )}
        </tbody>
      </table>
    </div>
  );
}
