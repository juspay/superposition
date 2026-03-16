// StatusBadge component
import type { ExperimentStatus } from "../types";

const statusColors: Record<ExperimentStatus, { bg: string; color: string }> = {
  CREATED: { bg: "#dbeafe", color: "#1e40af" },
  INPROGRESS: { bg: "#d1fae5", color: "#065f46" },
  PAUSED: { bg: "#fef3c7", color: "#92400e" },
  CONCLUDED: { bg: "#e5e7eb", color: "#374151" },
  DISCARDED: { bg: "#fee2e2", color: "#991b1b" },
};

export function StatusBadge({ status }: { status: ExperimentStatus }) {
  const style = statusColors[status] || statusColors.CREATED;
  return (
    <span
      style={{
        display: "inline-block",
        padding: "2px 10px",
        borderRadius: 12,
        fontSize: 12,
        fontWeight: 600,
        background: style.bg,
        color: style.color,
      }}
    >
      {status}
    </span>
  );
}
