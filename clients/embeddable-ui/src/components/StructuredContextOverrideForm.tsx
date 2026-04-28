import { useEffect, useMemo, useRef, useState } from "react";
import type { DefaultConfig, Dimension, JsonValue } from "../types";
import { ConditionBadges } from "./ConditionBadges";
import { buttonSecondary, inputStyle } from "./FormField";
import { Tooltip } from "./Tooltip";

export interface FieldEntryState {
  key: string;
  value: JsonValue;
  draft?: string;
  error?: string;
  locked?: boolean;
  required?: boolean;
}

interface StructuredContextOverrideFormProps {
  contextEntries: FieldEntryState[];
  overrideEntries: FieldEntryState[];
  dimensions: Dimension[];
  defaultConfigs: DefaultConfig[];
  lockedScope?: Record<string, JsonValue>;
  lockedKeys?: string[];
  showContextFields?: boolean;
  showOverrideFields?: boolean;
  showLockedScope?: boolean;
  showValidationErrors?: boolean;
  onAddContextKey: (key: string) => void;
  onUpdateContextEntry: (key: string, update: Partial<FieldEntryState>) => void;
  onRemoveContextKey: (key: string) => void;
  onAddOverrideKey: (key: string) => void;
  onUpdateOverrideEntry: (key: string, update: Partial<FieldEntryState>) => void;
  onRemoveOverrideKey: (key: string) => void;
}

type InputKind = "string" | "number" | "boolean" | "enum" | "json";

function getSchemaType(schema?: Record<string, JsonValue>): string | undefined {
  const raw = schema?.type;
  if (typeof raw === "string") return raw;
  if (Array.isArray(raw)) {
    return raw.find(
      (item): item is string => typeof item === "string" && item !== "null",
    );
  }
  return undefined;
}

function getEnumOptions(schema?: Record<string, JsonValue>): JsonValue[] {
  return Array.isArray(schema?.enum) ? schema.enum : [];
}

function getInputKind(schema?: Record<string, JsonValue>): InputKind {
  if (getEnumOptions(schema).length > 0) return "enum";

  const schemaType = getSchemaType(schema);
  if (schemaType === "integer" || schemaType === "number") return "number";
  if (schemaType === "boolean") return "boolean";
  if (schemaType === "object" || schemaType === "array") return "json";
  return "string";
}

function getDefaultValue(schema?: Record<string, JsonValue>): JsonValue {
  if (schema && "default" in schema) {
    return schema.default as JsonValue;
  }

  const enumOptions = getEnumOptions(schema);
  if (enumOptions.length > 0) return enumOptions[0] ?? "";

  switch (getSchemaType(schema)) {
    case "integer":
    case "number":
      return 0;
    case "boolean":
      return false;
    case "array":
      return [];
    case "object":
      return {};
    default:
      return "";
  }
}

function getTypeBadge(schema?: Record<string, JsonValue>) {
  return getEnumOptions(schema).length > 0 ? "enum" : (getSchemaType(schema) ?? "string");
}

function valueToDraft(value: JsonValue) {
  return JSON.stringify(value, null, 2);
}

function normalizeEntries(entries: FieldEntryState[]) {
  return entries.slice().sort((left, right) => {
    if (left.required && !right.required) return -1;
    if (!left.required && right.required) return 1;
    return left.key.localeCompare(right.key);
  });
}

function PlusIcon() {
  return (
    <svg
      aria-hidden="true"
      viewBox="0 0 20 20"
      width="var(--sp-search-icon-size)"
      height="var(--sp-search-icon-size)"
      style={{ color: "var(--sp-search-icon-color)", flex: "0 0 auto" }}
    >
      <path
        d="M10 4.5v11M4.5 10h11"
        fill="none"
        stroke="currentColor"
        strokeLinecap="round"
        strokeWidth="2"
      />
    </svg>
  );
}

function SearchIcon() {
  return (
    <svg
      aria-hidden="true"
      viewBox="0 0 20 20"
      width="var(--sp-icon-size)"
      height="var(--sp-icon-size)"
      style={{ color: "var(--sp-icon-color)", flex: "0 0 auto" }}
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

function TrashIcon() {
  return (
    <svg
      aria-hidden="true"
      viewBox="0 0 24 24"
      width="calc(var(--sp-icon-size) * 1.35)"
      height="calc(var(--sp-icon-size) * 1.35)"
      style={{ color: "currentColor", flex: "0 0 auto" }}
    >
      <path
        d="M17 6h5v2h-2v13a1 1 0 0 1-1 1H5a1 1 0 0 1-1-1V8H2V6h5V3a1 1 0 0 1 1-1h8a1 1 0 0 1 1 1v3ZM6 8v12h12V8H6Zm3 3h2v6H9v-6Zm4 0h2v6h-2v-6ZM9 4v2h6V4H9Z"
        fill="currentColor"
      />
    </svg>
  );
}

function AddOptionMenu({
  label,
  options,
  selectedValues,
  onSelect,
}: {
  label: string;
  options: Array<{ value: string; label: string }>;
  selectedValues: string[];
  onSelect: (value: string) => void;
}) {
  const [open, setOpen] = useState(false);
  const [search, setSearch] = useState("");
  const rootRef = useRef<HTMLDivElement | null>(null);
  const selectedSet = useMemo(() => new Set(selectedValues), [selectedValues]);
  const availableOptions = useMemo(() => {
    const query = search.trim().toLowerCase();
    return options.filter((option) => {
      if (selectedSet.has(option.value)) return false;
      if (!query) return true;
      return option.label.toLowerCase().includes(query);
    });
  }, [options, search, selectedSet]);

  useEffect(() => {
    if (!open) return;

    const handlePointerDown = (event: MouseEvent) => {
      if (!rootRef.current?.contains(event.target as Node)) {
        setOpen(false);
      }
    };

    document.addEventListener("mousedown", handlePointerDown);
    return () => document.removeEventListener("mousedown", handlePointerDown);
  }, [open]);

  return (
    <div ref={rootRef} style={{ display: "grid", gap: 8, width: "fit-content" }}>
      <button
        type="button"
        aria-haspopup="listbox"
        aria-expanded={open}
        style={{
          ...buttonSecondary,
          display: "inline-flex",
          alignItems: "center",
          gap: 8,
          color: "var(--sp-color-primary)",
          borderColor: "var(--sp-color-primary)",
          background: "var(--sp-color-panel)",
          boxShadow: open
            ? "0 0 0 3px color-mix(in oklab, var(--sp-color-primary) 12%, transparent)"
            : "none",
        }}
        onClick={() => {
          setOpen((current) => !current);
          setSearch("");
        }}
      >
        <PlusIcon />
        {label}
      </button>
      {open && (
        <div
          role="listbox"
          aria-label={label}
          style={{
            zIndex: 3,
            width: "min(var(--sp-dropdown-width), calc(100vw - 48px))",
            maxHeight: 260,
            overflowY: "auto",
            padding: 8,
            border: "1px solid var(--sp-dropdown-menu-border)",
            borderRadius: "var(--sp-dropdown-control-radius)",
            background: "var(--sp-dropdown-menu-bg)",
            boxShadow: "var(--sp-dropdown-menu-shadow)",
          }}
        >
          <div
            style={{
              display: "flex",
              alignItems: "center",
              gap: 8,
              border: "1px solid var(--sp-search-border)",
              borderRadius: "var(--sp-search-radius)",
              background: "var(--sp-search-bg)",
              padding: "0 var(--sp-space-sm)",
              marginBottom: 8,
            }}
          >
            <SearchIcon />
            <input
              aria-label={`${label} search`}
              style={{
                width: "100%",
                border: 0,
                outline: "none",
                background: "transparent",
                color: "var(--sp-search-text)",
                padding: "10px 0",
                fontSize: "var(--sp-search-font-size)",
              }}
              placeholder="Search"
              value={search}
              onChange={(event) => setSearch(event.target.value)}
              autoFocus
            />
          </div>
          {availableOptions.length === 0 ? (
            <div
              style={{
                padding: "10px 12px",
                color: "var(--sp-color-muted)",
                fontSize: 13,
              }}
            >
              No options available
            </div>
          ) : (
            availableOptions.map((option) => (
              <button
                key={option.value}
                type="button"
                role="option"
                aria-selected={false}
                style={{
                  width: "100%",
                  display: "block",
                  padding: "12px 14px",
                  border: 0,
                  borderRadius: "var(--sp-inline-radius)",
                  background: "transparent",
                  color: "var(--sp-color-text)",
                  cursor: "pointer",
                  fontWeight: 600,
                  textAlign: "left",
                  boxShadow: "none",
                }}
                onMouseEnter={(event) => {
                  event.currentTarget.style.background =
                    "var(--sp-dropdown-option-hover-bg)";
                }}
                onMouseLeave={(event) => {
                  event.currentTarget.style.background = "transparent";
                }}
                onClick={() => {
                  onSelect(option.value);
                  setOpen(false);
                  setSearch("");
                }}
              >
                {option.label}
              </button>
            ))
          )}
        </div>
      )}
    </div>
  );
}

function FieldCard({
  title,
  entries,
  addLabel,
  selectOptions,
  validationMessage,
  onAdd,
  onUpdate,
  onRemove,
  schemaFor,
}: {
  title: string;
  entries: FieldEntryState[];
  addLabel: string;
  selectOptions: Array<{ value: string; label: string }>;
  validationMessage?: string;
  onAdd: (key: string) => void;
  onUpdate: (key: string, update: Partial<FieldEntryState>) => void;
  onRemove: (key: string) => void;
  schemaFor: (key: string) => Record<string, JsonValue> | undefined;
}) {
  return (
    <div style={{ display: "grid", gap: 14 }}>
      <div style={{ display: "grid", gap: 4 }}>
        <div style={{ fontSize: 18, fontWeight: 700 }}>{title}</div>
        {validationMessage && (
          <div style={{ fontSize: 12, color: "var(--sp-feedback-danger-text)" }}>
            {validationMessage}
          </div>
        )}
      </div>

      <div
        style={{
          border:
            "1px solid color-mix(in oklab, var(--sp-color-border) 55%, transparent)",
          borderRadius: "var(--sp-card-radius)",
          background: "var(--sp-color-surface-muted)",
          padding: entries.length === 0 ? "42px 18px" : "var(--sp-space-lg)",
          display: "grid",
          gap: 18,
          justifyItems: entries.length === 0 ? "center" : "start",
        }}
      >
        {entries.length === 0 ? (
          <AddOptionMenu
            label={addLabel}
            options={selectOptions}
            selectedValues={[]}
            onSelect={onAdd}
          />
        ) : (
          <>
            {entries.map((entry) => {
              const schema = schemaFor(entry.key);
              const inputKind = getInputKind(schema);
              const enumOptions = getEnumOptions(schema);

              return (
                <div
                  key={entry.key}
                  style={{
                    width: "min(620px, 100%)",
                    display: "grid",
                    gridTemplateColumns: "minmax(0, 1fr) auto",
                    gap: "var(--sp-space-md)",
                    alignItems: "center",
                  }}
                >
                  <div style={{ display: "grid", gap: 10 }}>
                    <div
                      style={{
                        display: "flex",
                        alignItems: "center",
                        gap: 8,
                        flexWrap: "wrap",
                      }}
                    >
                      <span
                        style={{
                          fontSize: 14,
                          fontWeight: 700,
                          color: "var(--sp-color-muted)",
                        }}
                      >
                        {entry.key}:
                      </span>
                      <span
                        style={{
                          padding: "2px 8px",
                          borderRadius: "var(--sp-pill-radius)",
                          border: "1px solid var(--sp-color-border)",
                          fontSize: 11,
                          fontWeight: 700,
                          color: "var(--sp-color-muted)",
                        }}
                      >
                        {getTypeBadge(schema)}
                      </span>
                      {entry.required && (
                        <span
                          style={{
                            fontSize: 11,
                            fontWeight: 700,
                            color: "var(--sp-color-muted)",
                          }}
                        >
                          Required
                        </span>
                      )}
                    </div>

                    {inputKind === "string" && (
                      <input
                        aria-label={entry.key}
                        style={inputStyle}
                        value={
                          typeof entry.value === "string"
                            ? entry.value
                            : String(entry.value ?? "")
                        }
                        onChange={(event) =>
                          onUpdate(entry.key, {
                            value: event.target.value,
                            error: undefined,
                          })
                        }
                      />
                    )}

                    {inputKind === "number" && (
                      <input
                        aria-label={entry.key}
                        style={inputStyle}
                        type="number"
                        value={
                          typeof entry.value === "number"
                            ? entry.value
                            : Number(entry.value ?? 0)
                        }
                        onChange={(event) => {
                          const nextValue = event.target.value;
                          const parsed = Number(nextValue);
                          onUpdate(entry.key, {
                            value: nextValue === "" ? 0 : parsed,
                            error: Number.isNaN(parsed)
                              ? "Enter a valid number."
                              : undefined,
                          });
                        }}
                      />
                    )}

                    {inputKind === "boolean" && (
                      <select
                        aria-label={entry.key}
                        style={inputStyle}
                        value={String(Boolean(entry.value))}
                        onChange={(event) =>
                          onUpdate(entry.key, {
                            value: event.target.value === "true",
                            error: undefined,
                          })
                        }
                      >
                        <option value="true">true</option>
                        <option value="false">false</option>
                      </select>
                    )}

                    {inputKind === "enum" && (
                      <select
                        aria-label={entry.key}
                        style={inputStyle}
                        value={JSON.stringify(entry.value)}
                        onChange={(event) =>
                          onUpdate(entry.key, {
                            value: JSON.parse(event.target.value) as JsonValue,
                            error: undefined,
                          })
                        }
                      >
                        {enumOptions.map((option) => (
                          <option
                            key={JSON.stringify(option)}
                            value={JSON.stringify(option)}
                          >
                            {String(option)}
                          </option>
                        ))}
                      </select>
                    )}

                    {inputKind === "json" && (
                      <textarea
                        aria-label={entry.key}
                        style={{ ...inputStyle, fontFamily: "monospace", minHeight: 90 }}
                        value={entry.draft ?? valueToDraft(entry.value)}
                        onChange={(event) => {
                          const nextDraft = event.target.value;
                          try {
                            onUpdate(entry.key, {
                              draft: nextDraft,
                              value: JSON.parse(nextDraft) as JsonValue,
                              error: undefined,
                            });
                          } catch {
                            onUpdate(entry.key, {
                              draft: nextDraft,
                              error: "Enter valid JSON.",
                            });
                          }
                        }}
                      />
                    )}

                    {entry.error && (
                      <div
                        style={{
                          fontSize: 12,
                          color: "var(--sp-feedback-danger-text)",
                        }}
                      >
                        {entry.error}
                      </div>
                    )}
                  </div>
                  <Tooltip content={`Remove ${entry.key}`}>
                    <button
                      type="button"
                      aria-label={`Remove ${entry.key}`}
                      style={{
                        width: "var(--sp-form-remove-button-width)",
                        height: "var(--sp-form-remove-button-height)",
                        border: "1px solid var(--sp-form-remove-button-border)",
                        borderRadius: "var(--sp-form-remove-button-radius)",
                        background: "var(--sp-form-remove-button-bg)",
                        color: "var(--sp-form-remove-button-text)",
                        boxShadow: "var(--sp-form-remove-button-shadow)",
                        cursor:
                          entry.required || entry.locked ? "not-allowed" : "pointer",
                        opacity: entry.required || entry.locked ? 0.45 : 1,
                        padding: 0,
                        display: "inline-flex",
                        alignItems: "center",
                        justifyContent: "center",
                      }}
                      onClick={() => onRemove(entry.key)}
                      disabled={entry.required || entry.locked}
                    >
                      <TrashIcon />
                    </button>
                  </Tooltip>
                </div>
              );
            })}
            <AddOptionMenu
              label={addLabel}
              options={selectOptions}
              selectedValues={entries.map((entry) => entry.key)}
              onSelect={onAdd}
            />
          </>
        )}
      </div>
    </div>
  );
}

export function defaultEntryFromSchema(
  key: string,
  schema?: Record<string, JsonValue>,
  options?: { required?: boolean; locked?: boolean },
): FieldEntryState {
  const value = getDefaultValue(schema);
  return {
    key,
    value,
    draft: getInputKind(schema) === "json" ? valueToDraft(value) : undefined,
    error: undefined,
    required: options?.required,
    locked: options?.locked,
  };
}

export function StructuredContextOverrideForm({
  contextEntries,
  overrideEntries,
  dimensions,
  defaultConfigs,
  lockedScope,
  lockedKeys = [],
  showContextFields = true,
  showOverrideFields = true,
  showLockedScope = true,
  showValidationErrors = false,
  onAddContextKey,
  onUpdateContextEntry,
  onRemoveContextKey,
  onAddOverrideKey,
  onUpdateOverrideEntry,
  onRemoveOverrideKey,
}: StructuredContextOverrideFormProps) {
  const dimensionMap = useMemo(
    () =>
      Object.fromEntries(dimensions.map((dimension) => [dimension.dimension, dimension])),
    [dimensions],
  );
  const defaultConfigMap = useMemo(
    () => Object.fromEntries(defaultConfigs.map((config) => [config.key, config])),
    [defaultConfigs],
  );

  const availableDimensions = useMemo(
    () =>
      dimensions
        .filter((dimension) => dimension.dimension !== "variantIds")
        .map((dimension) => ({ value: dimension.dimension, label: dimension.dimension })),
    [dimensions],
  );

  const availableConfigKeys = useMemo(
    () => defaultConfigs.map((config) => ({ value: config.key, label: config.key })),
    [defaultConfigs],
  );

  return (
    <div style={{ display: "grid", gap: 18 }}>
      {showLockedScope && lockedScope && Object.keys(lockedScope).length > 0 && (
        <div
          style={{
            display: "grid",
            gap: "var(--sp-space-sm)",
            marginBottom: "var(--sp-space-sm)",
            padding: "var(--sp-space-md)",
            borderRadius: "var(--sp-control-radius)",
            background: "var(--sp-color-surface-muted)",
            border: "1px solid var(--sp-color-border)",
          }}
        >
          <div
            style={{
              fontSize: 12,
              fontWeight: 700,
              textTransform: "uppercase",
              color: "var(--sp-color-muted)",
            }}
          >
            Fixed Scope
          </div>
          <ConditionBadges condition={lockedScope} lockedKeys={lockedKeys} />
        </div>
      )}

      {showContextFields && (
        <FieldCard
          title="Context"
          entries={normalizeEntries(contextEntries)}
          addLabel="Add Context"
          selectOptions={availableDimensions}
          validationMessage={
            showValidationErrors && contextEntries.length === 0
              ? "Select at least one context condition."
              : undefined
          }
          onAdd={onAddContextKey}
          onUpdate={onUpdateContextEntry}
          onRemove={onRemoveContextKey}
          schemaFor={(key) => dimensionMap[key]?.schema}
        />
      )}

      {showOverrideFields && (
        <FieldCard
          title="Overrides"
          entries={overrideEntries}
          addLabel="Add Override"
          selectOptions={availableConfigKeys}
          validationMessage={
            showValidationErrors && overrideEntries.length === 0
              ? "Select at least one override value."
              : undefined
          }
          onAdd={onAddOverrideKey}
          onUpdate={onUpdateOverrideEntry}
          onRemove={onRemoveOverrideKey}
          schemaFor={(key) => defaultConfigMap[key]?.schema}
        />
      )}
    </div>
  );
}
