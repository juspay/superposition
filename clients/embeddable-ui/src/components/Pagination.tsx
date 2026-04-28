import React from "react";

export interface PaginationProps {
  currentPage: number;
  totalPages: number;
  onPageChange: (page: number) => void;
}

const btnStyle: React.CSSProperties = {
  padding: "var(--sp-compact-control-padding)",
  border: "1px solid var(--sp-button-secondary-border)",
  background: "var(--sp-button-secondary-bg)",
  color: "var(--sp-button-secondary-text)",
  borderRadius: "var(--sp-inline-radius)",
  cursor: "pointer",
  fontSize: 13,
  fontWeight: 600,
};

const activeBtnStyle: React.CSSProperties = {
  ...btnStyle,
  background: "var(--sp-feedback-info-bg)",
  color: "var(--sp-feedback-info-text)",
  borderColor: "var(--sp-feedback-info-border)",
};

export function Pagination({ currentPage, totalPages, onPageChange }: PaginationProps) {
  if (totalPages <= 1) return null;

  const pages: number[] = [];
  const start = Math.max(1, currentPage - 2);
  const end = Math.min(totalPages, currentPage + 2);
  for (let i = start; i <= end; i++) pages.push(i);

  return (
    <div
      style={{
        display: "flex",
        gap: 8,
        alignItems: "center",
        justifyContent: "space-between",
        padding: "10px 4px 0",
        flexWrap: "wrap",
      }}
    >
      <div style={{ fontSize: 13, color: "var(--sp-color-muted)" }}>
        Page {currentPage} of {totalPages}
      </div>
      <div
        style={{
          display: "flex",
          gap: 6,
          alignItems: "center",
          justifyContent: "center",
        }}
      >
        <button
          style={btnStyle}
          disabled={currentPage <= 1}
          onClick={() => onPageChange(currentPage - 1)}
        >
          Previous
        </button>
        {pages.map((p) => (
          <button
            key={p}
            style={p === currentPage ? activeBtnStyle : btnStyle}
            onClick={() => onPageChange(p)}
          >
            {p}
          </button>
        ))}
        <button
          style={btnStyle}
          disabled={currentPage >= totalPages}
          onClick={() => onPageChange(currentPage + 1)}
        >
          Next
        </button>
      </div>
    </div>
  );
}
