import { useMemo, useState } from "react";
import {
  buttonPrimary,
  ConditionBadges,
  JsonViewer,
  ScopedContextEditor,
} from "../components";
import { useAlerts } from "../providers/AlertProvider";
import { useSuperposition } from "../providers/SuperpositionProvider";
import type { JsonValue } from "../types";
import { mergeScopedContext, stripScopedDimensions } from "../utils/context-filter";

export function ResolveView() {
  const { resolve, config, scope } = useSuperposition();
  const { addAlert } = useAlerts();
  const [contextInput, setContextInput] = useState("{}");
  const [result, setResult] = useState<Record<string, JsonValue> | null>(null);
  const [loading, setLoading] = useState(false);

  const scopedContext = scope.effectiveContext;
  const lockedDims = scope.lockedDimensions;
  const readOnly = config.readOnly === true;

  const parsedContext = useMemo(() => {
    try {
      const value = JSON.parse(contextInput);
      if (!value || Array.isArray(value) || typeof value !== "object") {
        return { value: null, error: "Context must be a JSON object." };
      }

      return { value: value as Record<string, JsonValue>, error: null };
    } catch {
      return {
        value: null,
        error: 'Enter valid context JSON, for example {"dimension":"value"}.',
      };
    }
  }, [contextInput]);

  const handleResolve = async () => {
    if (parsedContext.error) {
      addAlert("error", parsedContext.error);
      return;
    }

    setLoading(true);
    try {
      const { context: additionalContext, removedKeys } = stripScopedDimensions(
        parsedContext.value as Record<string, JsonValue>,
        scopedContext,
      );
      const ctx = mergeScopedContext(additionalContext, scopedContext);
      const res = await resolve.resolve(ctx);
      setResult(res);

      if (removedKeys.length > 0) {
        addAlert(
          "info",
          `Locked scope applied automatically for: ${removedKeys.join(", ")}`,
        );
      }
    } catch (err) {
      addAlert("error", err instanceof Error ? err.message : String(err));
    } finally {
      setLoading(false);
    }
  };

  return (
    <div>
      <h2 style={{ margin: "0 0 16px", fontSize: 20, fontWeight: 600 }}>
        Resolve Config
      </h2>

      {scopedContext && Object.keys(scopedContext).length > 0 && (
        <div
          style={{
            marginBottom: 12,
            padding: "8px 12px",
            background: "var(--sp-feedback-warning-bg)",
            color: "var(--sp-feedback-warning-text)",
            borderRadius: "var(--sp-inline-radius)",
            fontSize: 13,
          }}
        >
          Scope: <ConditionBadges condition={scopedContext} lockedKeys={lockedDims} />
        </div>
      )}

      {readOnly && (
        <div
          style={{
            marginBottom: 12,
            padding: "10px 12px",
            borderRadius: "var(--sp-inline-radius)",
            background: "var(--sp-feedback-warning-bg)",
            border: "1px solid var(--sp-feedback-warning-border)",
            color: "var(--sp-feedback-warning-text)",
            fontSize: 13,
          }}
        >
          Read-only mode
        </div>
      )}

      <ScopedContextEditor
        value={contextInput}
        onChange={setContextInput}
        scopedContext={scopedContext}
        lockedKeys={lockedDims}
        error={parsedContext.error ?? undefined}
        minHeight={100}
      />

      <button
        style={buttonPrimary}
        onClick={handleResolve}
        disabled={loading || !!parsedContext.error}
      >
        {loading ? "Resolving..." : "Resolve"}
      </button>

      {result && (
        <div style={{ marginTop: 20 }}>
          <h3 style={{ fontSize: 16, fontWeight: 600, marginBottom: 8 }}>Result</h3>
          <JsonViewer data={result} collapsed={false} />
        </div>
      )}
    </div>
  );
}
