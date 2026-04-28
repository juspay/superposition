import { useCallback, useMemo, useState } from "react";
import {
  buttonDanger,
  buttonPrimary,
  buttonSecondary,
  FormField,
  inputStyle,
  JsonViewer,
  Modal,
  Pagination,
  Table,
} from "../components";
import type { Column } from "../components/Table";
import { useApi, useMutation } from "../hooks/useApi";
import { useAlerts } from "../providers/AlertProvider";
import { useSuperposition } from "../providers/SuperpositionProvider";
import type { CreateDefaultConfigRequest, DefaultConfig, JsonValue } from "../types";
import { requestConfirmation } from "../utils";

export interface ConfigManagerProps {
  /** Items per page */
  pageSize?: number;
  /** Restrict default config keys to one or more prefixes */
  prefix?: string | string[];
  /** Resolve and display values using the active host scope */
  showResolvedValues?: boolean;
}

function normalizePrefixes(prefix?: string | string[]): string[] | undefined {
  if (!prefix) return undefined;
  const prefixes = Array.isArray(prefix) ? prefix : [prefix];
  const normalized = prefixes.map((item) => item.trim()).filter(Boolean);
  return normalized.length > 0 ? normalized : undefined;
}

function keyMatchesPrefix(key: string, prefixes?: string[]): boolean {
  if (!prefixes || prefixes.length === 0) return true;
  return prefixes.some((prefix) => key.startsWith(prefix));
}

function applyResolvedValues(
  rows: DefaultConfig[],
  resolvedValues?: Record<string, JsonValue>,
): DefaultConfig[] {
  if (!resolvedValues) return rows;

  return rows.map((row) =>
    Object.prototype.hasOwnProperty.call(resolvedValues, row.key)
      ? { ...row, value: resolvedValues[row.key] }
      : row,
  );
}

export function ConfigManager({
  pageSize = 20,
  prefix,
  showResolvedValues = true,
}: ConfigManagerProps) {
  const { config, defaultConfigs, resolve, scope } = useSuperposition();
  const { addAlert } = useAlerts();
  const [page, setPage] = useState(1);
  const [search, setSearch] = useState("");
  const [showCreate, setShowCreate] = useState(false);
  const readOnly = config.readOnly === true;
  const prefixes = useMemo(
    () => normalizePrefixes(prefix ?? config.filters?.defaultConfigPrefix),
    [config.filters?.defaultConfigPrefix, prefix],
  );
  const resolvedContext = scope.effectiveContext ?? {};
  const resolvedContextKey = JSON.stringify(resolvedContext);

  const { data, loading, error, refetch } = useApi(
    () =>
      defaultConfigs.list(
        { page, count: pageSize },
        { name: search || undefined, prefix: prefixes },
      ),
    [page, pageSize, search, prefixes],
  );

  const { data: resolvedValues } = useApi(
    () =>
      showResolvedValues
        ? resolve.resolve(resolvedContext)
        : Promise.resolve({} as Record<string, JsonValue>),
    [resolve, showResolvedValues, resolvedContextKey],
  );

  // Create form state
  const [newKey, setNewKey] = useState("");
  const [newValue, setNewValue] = useState("null");
  const [newSchema, setNewSchema] = useState('{"type": "string"}');
  const [newDesc, setNewDesc] = useState("");
  const [newReason, setNewReason] = useState("");

  const parsedValue = useMemo(() => {
    const trimmedValue = newValue.trim();

    if (!trimmedValue) {
      return { value: "", error: null, isImplicitString: true };
    }

    try {
      return {
        value: JSON.parse(trimmedValue) as DefaultConfig["value"],
        error: null,
        isImplicitString: false,
      };
    } catch {
      return {
        value: newValue,
        error: null,
        isImplicitString: true,
      };
    }
  }, [newValue]);

  const parsedSchema = useMemo(() => {
    try {
      const value = JSON.parse(newSchema) as Record<string, unknown>;
      if (!value || Array.isArray(value) || typeof value !== "object") {
        return { value: null, error: "Schema must be a JSON object." };
      }

      return { value, error: null };
    } catch {
      return {
        value: null,
        error: 'Enter valid schema JSON, for example {"type":"string"}.',
      };
    }
  }, [newSchema]);

  const createMutation = useMutation(
    useCallback(
      async (req: CreateDefaultConfigRequest) => {
        const result = await defaultConfigs.create(req);
        addAlert("success", `Config "${result.key}" created`);
        return result;
      },
      [defaultConfigs, addAlert],
    ),
  );

  const createDisabled =
    readOnly ||
    createMutation.loading ||
    !newKey.trim() ||
    !newReason.trim() ||
    !!parsedSchema.error;

  const deleteMutation = useMutation(
    useCallback(
      async (key: string) => {
        await defaultConfigs.delete(key);
        addAlert("success", `Config "${key}" deleted`);
      },
      [defaultConfigs, addAlert],
    ),
  );

  const handleCreate = async () => {
    if (parsedValue.error) {
      addAlert("error", parsedValue.error);
      return;
    }

    if (parsedSchema.error) {
      addAlert("error", parsedSchema.error);
      return;
    }

    try {
      await createMutation.mutate({
        key: newKey.trim(),
        value: parsedValue.value,
        schema: parsedSchema.value as CreateDefaultConfigRequest["schema"],
        description: newDesc || "No description",
        change_reason: newReason.trim(),
      });
      setShowCreate(false);
      setNewKey("");
      setNewValue("null");
      setNewSchema('{"type": "string"}');
      setNewDesc("");
      setNewReason("");
      refetch();
    } catch (err) {
      addAlert(
        "error",
        err instanceof Error
          ? err.message
          : createMutation.error || "Failed to create config",
      );
    }
  };

  const handleDelete = async (key: string) => {
    const confirmed = await requestConfirmation(config.ui, {
      title: `Delete config "${key}"?`,
      description: "This removes the default config from the workspace.",
      confirmLabel: "Delete",
      cancelLabel: "Cancel",
    });
    if (!confirmed) return;

    try {
      await deleteMutation.mutate(key);
      refetch();
    } catch {
      addAlert("error", deleteMutation.error || "Failed to delete config");
    }
  };

  const columns: Column<DefaultConfig>[] = [
    { key: "key", header: "Key", width: "25%" },
    {
      key: "value",
      header: showResolvedValues ? "Resolved Value" : "Value",
      width: "25%",
      render: (row) => <JsonViewer data={row.value} />,
    },
    {
      key: "schema",
      header: "Schema",
      width: "20%",
      render: (row) => <JsonViewer data={row.schema} />,
    },
    { key: "description", header: "Description", width: "20%" },
    {
      key: "actions",
      header: "",
      width: "10%",
      render: (row) =>
        readOnly ? null : (
          <button
            style={buttonDanger}
            onClick={(e) => {
              e.stopPropagation();
              handleDelete(row.key);
            }}
          >
            Delete
          </button>
        ),
    },
  ];

  const rows = applyResolvedValues(
    (data?.data ?? []).filter((row) => keyMatchesPrefix(row.key, prefixes)),
    showResolvedValues ? (resolvedValues ?? undefined) : undefined,
  );
  const hasRows = rows.length > 0;

  return (
    <div style={{ display: "grid", gap: 20 }}>
      <div
        style={{
          display: "flex",
          justifyContent: "space-between",
          alignItems: "flex-start",
          gap: 16,
          flexWrap: "wrap",
        }}
      >
        <h2 style={{ margin: 0, fontSize: 24, lineHeight: 1.08, fontWeight: 700 }}>
          Configs
        </h2>
        {!readOnly && (
          <button style={buttonPrimary} onClick={() => setShowCreate(true)}>
            Create config
          </button>
        )}
      </div>

      <div
        style={{
          display: "flex",
          alignItems: "center",
          gap: 12,
          flexWrap: "wrap",
        }}
      >
        <input
          style={{ ...inputStyle, maxWidth: 340 }}
          placeholder="Search by key..."
          value={search}
          onChange={(e) => {
            setSearch(e.target.value);
            setPage(1);
          }}
        />
        <div style={{ fontSize: 13, color: "var(--sp-color-muted)" }}>
          {data?.total_items ?? 0} keys
        </div>
      </div>

      {error && (
        <div
          style={{
            marginBottom: 12,
            padding: "10px 12px",
            borderRadius: "var(--sp-inline-radius)",
            background: "var(--sp-feedback-danger-bg)",
            border: "1px solid var(--sp-feedback-danger-border)",
            color: "var(--sp-feedback-danger-text)",
            fontSize: 13,
          }}
        >
          Failed to load configs: {error}
        </div>
      )}

      {loading ? (
        <Table
          columns={columns}
          data={[]}
          keyExtractor={(r) => r.key}
          loading
          emptyMessage="No configs found"
        />
      ) : hasRows ? (
        <Table
          columns={columns}
          data={rows}
          keyExtractor={(r) => r.key}
          loading={false}
          emptyMessage="No configs found"
        />
      ) : (
        <div
          style={{
            border: "1px solid var(--sp-color-border)",
            borderRadius: "var(--sp-card-radius)",
            background: "var(--sp-color-surface-muted)",
            padding: "40px 28px",
            display: "grid",
            gap: 12,
            justifyItems: "start",
          }}
        >
          <div
            style={{
              padding: "8px 12px",
              borderRadius: "var(--sp-pill-radius)",
              background: "var(--sp-feedback-info-bg)",
              fontSize: 12,
              fontWeight: 700,
            }}
          >
            Empty registry
          </div>
          <div style={{ fontSize: 24, fontWeight: 700, maxWidth: 560, lineHeight: 1.12 }}>
            No configs
          </div>
          {!readOnly && (
            <button style={buttonPrimary} onClick={() => setShowCreate(true)}>
              Create config
            </button>
          )}
        </div>
      )}

      {data && hasRows && (
        <Pagination
          currentPage={page}
          totalPages={data.total_pages}
          onPageChange={setPage}
        />
      )}

      <Modal
        open={showCreate}
        onClose={() => setShowCreate(false)}
        title="Create Default Config"
        footer={
          <>
            <button style={buttonSecondary} onClick={() => setShowCreate(false)}>
              Cancel
            </button>
            <button
              style={{
                ...buttonPrimary,
                opacity: createDisabled ? 0.55 : 1,
                cursor: createDisabled ? "not-allowed" : "pointer",
              }}
              onClick={handleCreate}
              disabled={createDisabled}
            >
              {createMutation.loading ? "Creating..." : "Create"}
            </button>
          </>
        }
      >
        <FormField label="Key" required>
          <input
            style={inputStyle}
            value={newKey}
            onChange={(e) => setNewKey(e.target.value)}
            placeholder="config.key.name"
          />
        </FormField>
        <FormField label="Value" required error={parsedValue.error ?? undefined}>
          <textarea
            style={{ ...inputStyle, fontFamily: "monospace", minHeight: 60 }}
            value={newValue}
            onChange={(e) => setNewValue(e.target.value)}
            placeholder='JSON or plain text, for example "hello", 42, true, or hello'
          />
        </FormField>
        <p
          style={{ margin: "-8px 0 12px", fontSize: 12, color: "var(--sp-color-muted)" }}
        >
          Plain text is stored as a string.
        </p>
        <FormField label="Schema (JSON)" required error={parsedSchema.error ?? undefined}>
          <textarea
            style={{ ...inputStyle, fontFamily: "monospace", minHeight: 60 }}
            value={newSchema}
            onChange={(e) => setNewSchema(e.target.value)}
          />
        </FormField>
        <FormField label="Description">
          <input
            style={inputStyle}
            value={newDesc}
            onChange={(e) => setNewDesc(e.target.value)}
            placeholder="Short operator-facing description"
          />
        </FormField>
        <FormField label="Change Reason" required>
          <input
            style={inputStyle}
            value={newReason}
            onChange={(e) => setNewReason(e.target.value)}
            placeholder="Reason for this change"
          />
        </FormField>
        {createMutation.error && (
          <div
            style={{
              marginTop: 4,
              padding: "10px 12px",
              borderRadius: "var(--sp-inline-radius)",
              background: "var(--sp-feedback-danger-bg)",
              border: "1px solid var(--sp-feedback-danger-border)",
              color: "var(--sp-feedback-danger-text)",
              fontSize: 13,
            }}
          >
            {createMutation.error}
          </div>
        )}
      </Modal>
    </div>
  );
}
