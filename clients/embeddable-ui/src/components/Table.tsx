import React from "react";

export interface Column<T> {
  key: string;
  header: string;
  render?: (row: T) => React.ReactNode;
  width?: string;
  align?: "left" | "center" | "right";
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
  minWidth: "var(--sp-table-min-width)",
  borderCollapse: "collapse",
  fontSize: "1rem",
  background: "var(--sp-color-panel)",
};

const thStyle: React.CSSProperties = {
  textAlign: "left",
  padding: "var(--sp-table-header-padding)",
  borderBottom: "1px solid var(--sp-color-border)",
  fontWeight: "var(--sp-table-header-font-weight)",
  color: "var(--sp-table-header-text)",
  background: "var(--sp-table-header-bg)",
  fontSize: "var(--sp-table-header-font-size)",
  letterSpacing: 0,
  textTransform: "var(--sp-table-header-text-transform)",
};

const tdStyle: React.CSSProperties = {
  padding: "var(--sp-space-md)",
  borderBottom: "1px solid color-mix(in oklab, var(--sp-color-border) 75%, transparent)",
  color: "var(--sp-color-text)",
  verticalAlign: "top",
};

function toTitleCase(value: string) {
  if (!value.trim()) return value;

  const normalized = value
    .replace(/([a-z0-9])([A-Z])/g, "$1 $2")
    .replace(/[_-]+/g, " ")
    .replace(/\s+/g, " ")
    .trim();

  return normalized
    .split(" ")
    .map((word) => {
      const lower = word.toLowerCase();
      return lower.charAt(0).toUpperCase() + lower.slice(1);
    })
    .join(" ");
}

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
          padding: "calc(var(--sp-space-lg) * 2)",
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
              <th
                key={col.key}
                style={{ ...thStyle, width: col.width, textAlign: col.align ?? "left" }}
              >
                {toTitleCase(col.header)}
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
                  padding: "calc(var(--sp-space-lg) * 2) var(--sp-space-lg)",
                  fontSize: "1rem",
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
                  <td
                    key={col.key}
                    style={{ ...tdStyle, textAlign: col.align ?? "left" }}
                  >
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
