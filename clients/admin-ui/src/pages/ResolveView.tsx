import { useState } from "react";
import { useSuperposition } from "../providers/SuperpositionProvider";
import { useAlerts } from "../providers/AlertProvider";
import {
  FormField,
  inputStyle,
  buttonPrimary,
  JsonViewer,
  ConditionBadges,
} from "../components";
import { mergeScopedContext, getLockedDimensions } from "../utils/context-filter";
import type { JsonValue } from "../types";

export function ResolveView() {
  const { resolve, config } = useSuperposition();
  const { addAlert } = useAlerts();
  const [contextInput, setContextInput] = useState("{}");
  const [result, setResult] = useState<Record<string, JsonValue> | null>(null);
  const [loading, setLoading] = useState(false);

  const scopedContext = config.context;
  const lockedDims = getLockedDimensions(scopedContext);

  const handleResolve = async () => {
    setLoading(true);
    try {
      const ctx = mergeScopedContext(
        JSON.parse(contextInput),
        scopedContext,
      );
      const res = await resolve.resolve(ctx);
      setResult(res);
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
        <div style={{ marginBottom: 12, padding: "8px 12px", background: "#fef3c7", borderRadius: 6, fontSize: 13 }}>
          Scoped context (auto-applied):{" "}
          <ConditionBadges
            condition={scopedContext!}
            lockedKeys={lockedDims}
          />
        </div>
      )}

      <FormField label="Additional Context (JSON)">
        <textarea
          style={{ ...inputStyle, fontFamily: "monospace", minHeight: 100 }}
          value={contextInput}
          onChange={(e) => setContextInput(e.target.value)}
          placeholder='{"dimension": "value"}'
        />
      </FormField>

      <button
        style={buttonPrimary}
        onClick={handleResolve}
        disabled={loading}
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
