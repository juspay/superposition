// StatusBadge component
import type { ExperimentStatus } from "../types";

const statusColors: Record<ExperimentStatus, { bg: string; color: string }> = {
  CREATED: { bg: "var(--sp-feedback-info-bg)", color: "var(--sp-feedback-info-text)" },
  INPROGRESS: {
    bg: "var(--sp-feedback-success-bg)",
    color: "var(--sp-feedback-success-text)",
  },
  PAUSED: {
    bg: "var(--sp-feedback-warning-bg)",
    color: "var(--sp-feedback-warning-text)",
  },
  CONCLUDED: {
    bg: "var(--sp-feedback-neutral-bg)",
    color: "var(--sp-feedback-neutral-text)",
  },
  DISCARDED: {
    bg: "var(--sp-feedback-danger-bg)",
    color: "var(--sp-feedback-danger-text)",
  },
};

export function StatusBadge({ status }: { status: ExperimentStatus }) {
  const style = statusColors[status] || statusColors.CREATED;
  return (
    <span
      style={{
        display: "inline-block",
        padding: "2px 10px",
        borderRadius: "var(--sp-pill-radius)",
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
