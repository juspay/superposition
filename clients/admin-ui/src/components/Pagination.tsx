import React from "react";

export interface PaginationProps {
  currentPage: number;
  totalPages: number;
  onPageChange: (page: number) => void;
}

const btnStyle: React.CSSProperties = {
  padding: "6px 12px",
  border: "1px solid #d1d5db",
  background: "#fff",
  borderRadius: 4,
  cursor: "pointer",
  fontSize: 13,
};

const activeBtnStyle: React.CSSProperties = {
  ...btnStyle,
  background: "#4f46e5",
  color: "#fff",
  borderColor: "#4f46e5",
};

export function Pagination({
  currentPage,
  totalPages,
  onPageChange,
}: PaginationProps) {
  if (totalPages <= 1) return null;

  const pages: number[] = [];
  const start = Math.max(1, currentPage - 2);
  const end = Math.min(totalPages, currentPage + 2);
  for (let i = start; i <= end; i++) pages.push(i);

  return (
    <div style={{ display: "flex", gap: 4, alignItems: "center", justifyContent: "center", padding: "12px 0" }}>
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
  );
}
