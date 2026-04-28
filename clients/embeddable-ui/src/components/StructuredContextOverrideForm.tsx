import { useMemo } from "react";
import type { DefaultConfig, Dimension, JsonValue } from "../types";
import { ConditionBadges } from "./ConditionBadges";
import { buttonSecondary, inputStyle } from "./FormField";

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

function FieldCard({
  title,
  entries,
  emptyLabel,
  addLabel,
  selectLabel,
  selectOptions,
  onAdd,
  onUpdate,
  onRemove,
  schemaFor,
}: {
  title: string;
  entries: FieldEntryState[];
  emptyLabel: string;
  addLabel: string;
  selectLabel: string;
  selectOptions: Array<{ value: string; label: string }>;
  onAdd: (key: string) => void;
  onUpdate: (key: string, update: Partial<FieldEntryState>) => void;
  onRemove: (key: string) => void;
  schemaFor: (key: string) => Record<string, JsonValue> | undefined;
}) {
  const selectId = `${title.toLowerCase().replace(/\s+/g, "-")}-select`;

  return (
    <div style={{ display: "grid", gap: 16 }}>
      <div
        style={{
          display: "flex",
          justifyContent: "space-between",
          alignItems: "center",
          gap: 12,
          flexWrap: "wrap",
        }}
      >
        <div style={{ fontSize: 16, fontWeight: 700 }}>{title}</div>
        <div style={{ display: "flex", gap: 8, alignItems: "center", flexWrap: "wrap" }}>
          <label
            htmlFor={selectId}
            style={{ fontSize: 13, color: "var(--sp-color-muted)" }}
          >
            {selectLabel}
          </label>
          <select
            id={selectId}
            aria-label={selectLabel}
            style={{ ...inputStyle, width: 220, padding: "8px 10px" }}
            defaultValue=""
            onChange={(event) => {
              const value = event.target.value;
              if (!value) return;
              onAdd(value);
              event.target.value = "";
            }}
          >
            <option value="">{addLabel}</option>
            {selectOptions.map((option) => (
              <option key={option.value} value={option.value}>
                {option.label}
              </option>
            ))}
          </select>
        </div>
      </div>

      <div
        style={{
          border: "1px solid var(--sp-color-border)",
          borderRadius: "var(--sp-card-radius)",
          background: "var(--sp-color-surface-muted)",
          padding: 18,
          display: "grid",
          gap: 16,
        }}
      >
        {entries.length === 0 ? (
          <div style={{ color: "var(--sp-color-muted)", fontSize: 13 }}>{emptyLabel}</div>
        ) : (
          entries.map((entry) => {
            const schema = schemaFor(entry.key);
            const inputKind = getInputKind(schema);
            const enumOptions = getEnumOptions(schema);

            return (
              <div key={entry.key} style={{ display: "grid", gap: 10 }}>
                <div
                  style={{
                    display: "flex",
                    justifyContent: "space-between",
                    alignItems: "center",
                    gap: 12,
                    flexWrap: "wrap",
                  }}
                >
                  <div
                    style={{
                      display: "flex",
                      alignItems: "center",
                      gap: 8,
                      flexWrap: "wrap",
                    }}
                  >
                    <span style={{ fontSize: 14, fontWeight: 700 }}>{entry.key}</span>
                    <span
                      style={{
                        padding: "2px 8px",
                        borderRadius: "var(--sp-pill-radius)",
                        border: "1px solid var(--sp-color-border)",
                        fontSize: 11,
                        textTransform: "uppercase",
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
                  <button
                    type="button"
                    style={{
                      ...buttonSecondary,
                      padding: "6px 10px",
                      fontSize: 12,
                      opacity: entry.required || entry.locked ? 0.55 : 1,
                    }}
                    onClick={() => onRemove(entry.key)}
                    disabled={entry.required || entry.locked}
                  >
                    Remove
                  </button>
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
                      onUpdate(entry.key, { value: event.target.value, error: undefined })
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
                        error: Number.isNaN(parsed) ? "Enter a valid number." : undefined,
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
                      <option key={JSON.stringify(option)} value={JSON.stringify(option)}>
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
                  <div style={{ fontSize: 12, color: "var(--sp-feedback-danger-text)" }}>
                    {entry.error}
                  </div>
                )}
              </div>
            );
          })
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
        .filter(
          (dimension) =>
            !contextEntries.some((entry) => entry.key === dimension.dimension),
        )
        .map((dimension) => ({ value: dimension.dimension, label: dimension.dimension })),
    [contextEntries, dimensions],
  );

  const availableConfigKeys = useMemo(
    () =>
      defaultConfigs
        .filter((config) => !overrideEntries.some((entry) => entry.key === config.key))
        .map((config) => ({ value: config.key, label: config.key })),
    [defaultConfigs, overrideEntries],
  );

  return (
    <div style={{ display: "grid", gap: 18 }}>
      {lockedScope && Object.keys(lockedScope).length > 0 && (
        <div style={{ display: "grid", gap: 8 }}>
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
          emptyLabel="No additional context conditions"
          addLabel="Add Context"
          selectLabel="Dimension"
          selectOptions={availableDimensions}
          onAdd={onAddContextKey}
          onUpdate={onUpdateContextEntry}
          onRemove={onRemoveContextKey}
          schemaFor={(key) => dimensionMap[key]?.schema}
        />
      )}

      <FieldCard
        title="Overrides"
        entries={overrideEntries}
        emptyLabel="No override values"
        addLabel="Add Override"
        selectLabel="Config Key"
        selectOptions={availableConfigKeys}
        onAdd={onAddOverrideKey}
        onUpdate={onUpdateOverrideEntry}
        onRemove={onRemoveOverrideKey}
        schemaFor={(key) => defaultConfigMap[key]?.schema}
      />
    </div>
  );
}
