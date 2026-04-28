export interface SearchFieldProps {
  value: string;
  onChange: (value: string) => void;
  placeholder: string;
  ariaLabel?: string;
}

function SearchIcon() {
  return (
    <svg
      aria-hidden="true"
      viewBox="0 0 20 20"
      width="var(--sp-search-icon-size)"
      height="var(--sp-search-icon-size)"
      style={{ color: "var(--sp-search-icon-color)", flex: "0 0 auto" }}
    >
      <circle
        cx="8.7"
        cy="8.7"
        r="5.2"
        fill="none"
        stroke="currentColor"
        strokeWidth="1.8"
      />
      <path
        d="m12.6 12.6 3.2 3.2"
        fill="none"
        stroke="currentColor"
        strokeLinecap="round"
        strokeWidth="1.8"
      />
    </svg>
  );
}

export function SearchField({
  value,
  onChange,
  placeholder,
  ariaLabel = placeholder,
}: SearchFieldProps) {
  return (
    <label
      style={{
        width: "var(--sp-search-width)",
        minHeight: "var(--sp-search-height)",
        display: "flex",
        alignItems: "center",
        gap: "var(--sp-space-sm)",
        padding: "var(--sp-search-padding)",
        border: "1px solid var(--sp-search-border)",
        borderRadius: "var(--sp-search-radius)",
        background: "var(--sp-search-bg)",
        color: "var(--sp-search-text)",
        boxShadow: "var(--sp-search-shadow)",
      }}
    >
      <SearchIcon />
      <input
        className="sp-search-input"
        aria-label={ariaLabel}
        style={{
          width: "100%",
          minWidth: 0,
          border: 0,
          outline: "none",
          background: "transparent",
          color: "var(--sp-search-text)",
          fontSize: "var(--sp-search-font-size)",
          fontWeight: "var(--sp-search-font-weight)",
        }}
        placeholder={placeholder}
        value={value}
        onChange={(event) => onChange(event.target.value)}
      />
    </label>
  );
}
