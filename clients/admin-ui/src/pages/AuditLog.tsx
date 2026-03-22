import React, { useMemo, useState } from "react";
import { useSuperposition } from "../providers/SuperpositionProvider";
import { useApi } from "../hooks/useApi";
import { Table, Pagination, JsonViewer } from "../components";
import type { Column } from "../components/Table";
import type { AuditLogEntry, EventAction } from "../types";
import { formatDateTime } from "../utils/format";

export interface AuditLogProps {
  pageSize?: number;
}

const actionColors: Record<EventAction, React.CSSProperties> = {
  INSERT: { color: "#065f46", background: "#d1fae5" },
  UPDATE: { color: "#1e40af", background: "#dbeafe" },
  DELETE: { color: "#991b1b", background: "#fee2e2" },
};

export function AuditLog({ pageSize = 20 }: AuditLogProps) {
  const { audit } = useSuperposition();
  const [page, setPage] = useState(1);
  const [actionFilter, setActionFilter] = useState<EventAction[]>([]);

  const { data, loading } = useApi(
    () =>
      audit.list(
        { page, count: pageSize },
        { action: actionFilter.length > 0 ? actionFilter : undefined },
      ),
    [page, pageSize, actionFilter],
  );

  const columns: Column<AuditLogEntry>[] = useMemo(
    () => [
      {
        key: "timestamp",
        header: "Time",
        width: "15%",
        render: (row) => formatDateTime(row.timestamp),
      },
      {
        key: "action",
        header: "Action",
        width: "10%",
        render: (row) => (
          <span
            style={{
              ...actionColors[row.action],
              padding: "2px 8px",
              borderRadius: 12,
              fontSize: 12,
              fontWeight: 600,
            }}
          >
            {row.action}
          </span>
        ),
      },
      { key: "table_name", header: "Table", width: "12%" },
      { key: "user_name", header: "User", width: "13%" },
      {
        key: "new_data",
        header: "New Data",
        width: "25%",
        render: (row) => row.new_data ? <JsonViewer data={row.new_data} /> : "—",
      },
      {
        key: "original_data",
        header: "Original Data",
        width: "25%",
        render: (row) =>
          row.original_data ? <JsonViewer data={row.original_data} /> : "—",
      },
    ],
    [],
  );

  return (
    <div>
      <h2 style={{ margin: "0 0 16px", fontSize: 20, fontWeight: 600 }}>Audit Log</h2>

      <div style={{ marginBottom: 12, display: "flex", gap: 6 }}>
        {(["INSERT", "UPDATE", "DELETE"] as EventAction[]).map((a) => (
          <button
            key={a}
            style={{
              padding: "4px 10px",
              fontSize: 12,
              border: "1px solid #d1d5db",
              borderRadius: 4,
              cursor: "pointer",
              ...actionColors[a],
              opacity: actionFilter.length === 0 || actionFilter.includes(a) ? 1 : 0.4,
            }}
            onClick={() =>
              setActionFilter((prev) =>
                prev.includes(a) ? prev.filter((x) => x !== a) : [...prev, a],
              )
            }
          >
            {a}
          </button>
        ))}
      </div>

      <Table
        columns={columns}
        data={data?.data ?? []}
        keyExtractor={(r) => r.id}
        loading={loading}
        emptyMessage="No audit entries"
      />

      {data && (
        <Pagination
          currentPage={page}
          totalPages={data.total_pages}
          onPageChange={setPage}
        />
      )}
    </div>
  );
}
