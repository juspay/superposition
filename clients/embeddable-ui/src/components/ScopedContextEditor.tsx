import type { Condition } from "../types";
import { ConditionBadges } from "./ConditionBadges";
import { FormField, inputStyle } from "./FormField";

export interface ScopedContextEditorProps {
  value: string;
  onChange: (value: string) => void;
  scopedContext?: Condition;
  lockedKeys?: string[];
  disabled?: boolean;
  label?: string;
  error?: string;
  minHeight?: number;
  placeholder?: string;
}

export function ScopedContextEditor({
  value,
  onChange,
  scopedContext,
  lockedKeys = [],
  disabled,
  label = "Additional Context (JSON)",
  error,
  minHeight = 80,
  placeholder = '{"dimension": "value"}',
}: ScopedContextEditorProps) {
  const hasScopedContext = !!scopedContext && Object.keys(scopedContext).length > 0;

  return (
    <div style={{ display: "grid", gap: hasScopedContext ? 12 : 0 }}>
      {hasScopedContext && (
        <div
          style={{
            padding: "14px 16px",
            borderRadius: "var(--sp-control-radius)",
            border: "1px solid var(--sp-color-border)",
            background: "var(--sp-color-surface-muted)",
            display: "grid",
            gap: 8,
          }}
        >
          <div
            style={{
              fontSize: 12,
              fontWeight: 700,
              letterSpacing: "0.04em",
              textTransform: "uppercase",
              color: "var(--sp-color-muted)",
            }}
          >
            Fixed embed scope
          </div>
          <ConditionBadges condition={scopedContext} lockedKeys={lockedKeys} />
        </div>
      )}

      <FormField label={label} error={error} disabled={disabled}>
        <textarea
          style={{ ...inputStyle, fontFamily: "monospace", minHeight }}
          value={value}
          onChange={(event) => onChange(event.target.value)}
          placeholder={placeholder}
        />
      </FormField>
    </div>
  );
}
