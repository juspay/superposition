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
};

const thStyle: React.CSSProperties = {
  textAlign: "left",
  padding: "10px 12px",
  borderBottom: "2px solid #e5e7eb",
  fontWeight: 600,
  color: "#374151",
  background: "#f9fafb",
};

const tdStyle: React.CSSProperties = {
  padding: "10px 12px",
  borderBottom: "1px solid #e5e7eb",
  color: "#1f2937",
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
      <div style={{ padding: 32, textAlign: "center", color: "#6b7280" }}>
        Loading...
      </div>
    );
  }

  return (
    <div style={{ overflowX: "auto" }}>
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
                style={{ ...tdStyle, textAlign: "center", color: "#9ca3af" }}
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
