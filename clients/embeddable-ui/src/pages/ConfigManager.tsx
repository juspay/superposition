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
  SearchField,
  Table,
} from "../components";
import type { Column } from "../components/Table";
import { useApi, useMutation } from "../hooks/useApi";
import { useAlerts } from "../providers/AlertProvider";
import { useSuperposition } from "../providers/SuperpositionUIProvider";
import type { CreateDefaultConfigRequest, DefaultConfig, JsonValue } from "../types";
import { matchesPrefix, normalizeFilterValues } from "../utils";
import {
  canUseFeatureAction,
  FeatureUnavailable,
  getMessage,
  isFeatureEnabled,
} from "./FeatureGate";

export interface ConfigManagerProps {
  /** Items per page */
  pageSize?: number;
  /** Restrict default config keys to one or more prefixes */
  prefix?: string | string[];
  /** Resolve and display values using the active host scope */
  showResolvedValues?: boolean;
  /** Allow create/delete controls for default configs. Defaults to view-only. */
  editable?: boolean;
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

function ConfigManagerContent({
  pageSize = 20,
  prefix,
  showResolvedValues = true,
  editable = false,
}: ConfigManagerProps) {
  const { config, defaultConfigs, resolve, scope } = useSuperposition();
  const { addAlert, confirmAction } = useAlerts();
  const [page, setPage] = useState(1);
  const [search, setSearch] = useState("");
  const [showCreate, setShowCreate] = useState(false);
  const canCreate = editable && canUseFeatureAction(config, "config", "create");
  const canDelete = editable && canUseFeatureAction(config, "config", "delete");
  const prefixes = useMemo(
    () => normalizeFilterValues(prefix ?? config.filters?.defaultConfigPrefix),
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
    [defaultConfigs, page, pageSize, search, prefixes],
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
    !canCreate ||
    createMutation.loading ||
    !newKey.trim() ||
    !newReason.trim() ||
    (newKey.trim() ? !matchesPrefix(newKey.trim(), prefixes) : false) ||
    !!parsedSchema.error;
  const keyPrefixError =
    newKey.trim() && !matchesPrefix(newKey.trim(), prefixes)
      ? `Key must start with ${prefixes?.join(" or ")}`
      : undefined;

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
    if (keyPrefixError) {
      addAlert("error", keyPrefixError);
      return;
    }

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
    const confirmed = await confirmAction({
      title: `Delete config "${key}"?`,
      description: "This removes the default config from the workspace.",
      confirmLabel: "Delete",
      cancelLabel: "Cancel",
      variant: "destructive",
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
        !canDelete ? null : (
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
    (data?.data ?? []).filter((row) => matchesPrefix(row.key, prefixes)),
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
        <h2
          style={{
            margin: "var(--sp-page-title-margin)",
            fontSize: "var(--sp-page-title-font-size)",
            lineHeight: 1.08,
            fontWeight: "var(--sp-page-title-font-weight)",
            color: "var(--sp-page-title-text)",
          }}
        >
          Configs
        </h2>
        {canCreate && (
          <button style={buttonPrimary} onClick={() => setShowCreate(true)}>
            {getMessage(config, "config.create", "Create config")}
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
        <SearchField
          placeholder="Search by key"
          value={search}
          onChange={(nextSearch) => {
            setSearch(nextSearch);
            setPage(1);
          }}
        />
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

      <Table
        columns={columns}
        data={loading ? [] : rows}
        keyExtractor={(r) => r.key}
        loading={loading}
        emptyMessage="No configs found"
      />

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
        <FormField label="Key" required error={keyPrefixError}>
          <input
            style={inputStyle}
            value={newKey}
            onChange={(e) => setNewKey(e.target.value)}
            placeholder={
              prefixes?.[0] ? `${prefixes[0]}config.key.name` : "config.key.name"
            }
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

export function ConfigManager(props: ConfigManagerProps) {
  const { config } = useSuperposition();

  if (!isFeatureEnabled(config.features, "config")) {
    return (
      <FeatureUnavailable
        feature="Configs"
        message={getMessage(
          config,
          "feature.disabled",
          "{feature} is not enabled for this embed.",
          { feature: "Configs" },
        )}
      />
    );
  }

  return <ConfigManagerContent {...props} />;
}
