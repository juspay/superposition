import type { Dispatch, SetStateAction } from "react";
import { useCallback, useMemo, useState } from "react";
import type { FieldEntryState } from "../components";
import {
  buttonDanger,
  buttonPrimary,
  buttonSecondary,
  ConditionBadges,
  defaultEntryFromSchema,
  FormField,
  inputStyle,
  JsonViewer,
  Modal,
  Pagination,
  StructuredContextOverrideForm,
  Table,
} from "../components";
import type { Column } from "../components/Table";
import { useApi, useMutation } from "../hooks/useApi";
import { useAlerts } from "../providers/AlertProvider";
import { useSuperposition } from "../providers/SuperpositionProvider";
import type { ContextOverride, PutContextRequest } from "../types";
import { requestConfirmation } from "../utils";
import {
  filterOverridesByScope,
  mergeScopedContext,
  stripScopedDimensions,
} from "../utils/context-filter";

export interface OverrideManagerProps {
  pageSize?: number;
  /** Restrict overrideable default config keys to one or more prefixes */
  defaultConfigPrefix?: string | string[];
  /**
   * Allow editing additional context dimensions in the create form.
   * Defaults to false when a host scope is attached.
   */
  allowContextEditing?: boolean;
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

export function OverrideManager({
  pageSize = 20,
  defaultConfigPrefix,
  allowContextEditing,
}: OverrideManagerProps) {
  const { overrides, config, scope, dimensions, defaultConfigs } = useSuperposition();
  const { addAlert } = useAlerts();
  const [page, setPage] = useState(1);
  const [search, setSearch] = useState("");
  const [showCreate, setShowCreate] = useState(false);

  const scopedContext = scope.effectiveContext;
  const hasScopedContext = Boolean(
    scopedContext && Object.keys(scopedContext).length > 0,
  );
  const canEditContext = allowContextEditing ?? !hasScopedContext;
  const readOnly = config.readOnly === true;
  const lockDimensions = config.scope?.locked !== false;
  const lockedDims = lockDimensions ? scope.lockedDimensions : [];
  const defaultConfigPrefixes = useMemo(
    () => normalizePrefixes(defaultConfigPrefix ?? config.filters?.defaultConfigPrefix),
    [config.filters?.defaultConfigPrefix, defaultConfigPrefix],
  );

  const { data, loading, error, refetch } = useApi(
    () => overrides.list({ page, count: pageSize }, { plaintext: search || undefined }),
    [page, pageSize, search],
  );

  const { data: dimensionsData } = useApi(() => dimensions.list({ all: true }), []);

  const { data: defaultConfigsData } = useApi(
    () => defaultConfigs.list({ all: true }),
    [],
  );

  const filteredData = useMemo(() => {
    if (!data) return [];
    return filterOverridesByScope(data.data, scopedContext);
  }, [data, scopedContext]);

  const [contextEntries, setContextEntries] = useState<FieldEntryState[]>([]);
  const [overrideEntries, setOverrideEntries] = useState<FieldEntryState[]>([]);
  const [newDesc, setNewDesc] = useState("");
  const [newReason, setNewReason] = useState("");

  const dimensionOptions = useMemo(
    () =>
      (dimensionsData?.data ?? []).filter(
        (dimension) => dimension.dimension !== "variantIds",
      ),
    [dimensionsData?.data],
  );

  const defaultConfigOptions = useMemo(
    () =>
      (defaultConfigsData?.data ?? []).filter((item) =>
        keyMatchesPrefix(item.key, defaultConfigPrefixes),
      ),
    [defaultConfigPrefixes, defaultConfigsData?.data],
  );

  const initializeContextEntries = useCallback(() => {
    if (!canEditContext) return [];

    return dimensionOptions
      .filter((dimension) => dimension.mandatory)
      .filter((dimension) => !(scopedContext && dimension.dimension in scopedContext))
      .map((dimension) =>
        defaultEntryFromSchema(dimension.dimension, dimension.schema, { required: true }),
      );
  }, [canEditContext, dimensionOptions, scopedContext]);

  const updateEntryState = useCallback(
    (
      setter: Dispatch<SetStateAction<FieldEntryState[]>>,
      key: string,
      update: Partial<FieldEntryState>,
    ) => {
      setter((current) =>
        current.map((entry) => (entry.key === key ? { ...entry, ...update } : entry)),
      );
    },
    [],
  );

  const openCreateModal = useCallback(() => {
    setContextEntries(initializeContextEntries());
    setOverrideEntries([]);
    setNewDesc("");
    setNewReason("");
    setShowCreate(true);
  }, [initializeContextEntries]);

  const contextObject = useMemo(
    () =>
      Object.fromEntries(
        contextEntries.map((entry) => [entry.key, entry.value]),
      ) as PutContextRequest["context"],
    [contextEntries],
  );

  const overrideObject = useMemo(
    () =>
      Object.fromEntries(
        overrideEntries.map((entry) => [entry.key, entry.value]),
      ) as PutContextRequest["override"],
    [overrideEntries],
  );

  const parsedContext = useMemo(() => {
    const invalidEntry = contextEntries.find((entry) => entry.error);
    if (invalidEntry) {
      return {
        value: null,
        error: invalidEntry.error ?? "Context contains an invalid value.",
      };
    }

    return { value: contextObject, error: null };
  }, [contextEntries, contextObject]);

  const parsedOverride = useMemo(() => {
    const invalidEntry = overrideEntries.find((entry) => entry.error);
    if (invalidEntry) {
      return {
        value: null,
        error: invalidEntry.error ?? "Overrides contain an invalid value.",
      };
    }

    if (overrideEntries.length === 0) {
      return { value: null, error: "Add at least one override value." };
    }

    return { value: overrideObject, error: null };
  }, [overrideEntries, overrideObject]);

  const createMutation = useMutation(
    useCallback(
      async (req: PutContextRequest) => {
        const result = await overrides.create(req);
        addAlert("success", "Override created");
        return result;
      },
      [overrides, addAlert],
    ),
  );

  const deleteMutation = useMutation(
    useCallback(
      async (id: string) => {
        await overrides.delete(id);
        addAlert("success", "Override deleted");
      },
      [overrides, addAlert],
    ),
  );

  const handleCreate = async () => {
    if (parsedContext.error) {
      addAlert("error", parsedContext.error);
      return;
    }

    if (parsedOverride.error) {
      addAlert("error", parsedOverride.error);
      return;
    }

    try {
      const { context: additionalContext, removedKeys } = stripScopedDimensions(
        parsedContext.value as PutContextRequest["context"],
        scopedContext,
      );
      const ctx = mergeScopedContext(additionalContext, scopedContext);
      await createMutation.mutate({
        context: ctx,
        override: parsedOverride.value as PutContextRequest["override"],
        description: newDesc || undefined,
        change_reason: newReason || "Created via admin UI",
      });

      if (removedKeys.length > 0) {
        addAlert(
          "info",
          `Locked scope applied automatically for: ${removedKeys.join(", ")}`,
        );
      }

      setShowCreate(false);
      setContextEntries(initializeContextEntries());
      setOverrideEntries([]);
      setNewDesc("");
      setNewReason("");
      refetch();
    } catch {
      addAlert("error", createMutation.error || "Failed to create override");
    }
  };

  const handleDelete = async (id: string) => {
    const confirmed = await requestConfirmation(config.ui, {
      title: "Delete this override?",
      description: "This removes the context override from the workspace.",
      confirmLabel: "Delete",
      cancelLabel: "Cancel",
    });
    if (!confirmed) return;

    try {
      await deleteMutation.mutate(id);
      refetch();
    } catch {
      addAlert("error", deleteMutation.error || "Failed to delete");
    }
  };

  const columns: Column<ContextOverride>[] = [
    {
      key: "value",
      header: "Context",
      width: "35%",
      render: (row) => <ConditionBadges condition={row.value} lockedKeys={lockedDims} />,
    },
    {
      key: "override_",
      header: "Overrides",
      width: "35%",
      render: (row) => <JsonViewer data={row.override_} />,
    },
    { key: "description", header: "Description", width: "15%" },
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
              handleDelete(row.id);
            }}
          >
            Delete
          </button>
        ),
    },
  ];

  const rows = filteredData;
  const hasRows = rows.length > 0;
  const createDisabled =
    readOnly ||
    createMutation.loading ||
    !newReason.trim() ||
    overrideEntries.length === 0 ||
    !!parsedContext.error ||
    !!parsedOverride.error;

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
          Overrides
        </h2>
        {!readOnly && (
          <button style={buttonPrimary} onClick={openCreateModal}>
            Create override
          </button>
        )}
      </div>

      {(readOnly || hasScopedContext) && (
        <div style={{ display: "grid", gap: 10 }}>
          {readOnly && (
            <div
              style={{
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
          {hasScopedContext && scopedContext && (
            <div
              style={{
                padding: "12px 14px",
                borderRadius: "var(--sp-control-radius)",
                background: "var(--sp-feedback-warning-bg)",
                border: "1px solid var(--sp-feedback-warning-border)",
                color: "var(--sp-feedback-warning-text)",
                fontSize: 13,
                display: "grid",
                gap: 8,
              }}
            >
              <div style={{ fontWeight: 700 }}>Fixed Scope</div>
              <ConditionBadges condition={scopedContext} lockedKeys={lockedDims} />
            </div>
          )}
        </div>
      )}

      <div
        style={{
          display: "flex",
          justifyContent: "space-between",
          alignItems: "center",
          gap: 12,
          flexWrap: "wrap",
        }}
      >
        <input
          style={{ ...inputStyle, maxWidth: 300 }}
          placeholder="Search overrides..."
          value={search}
          onChange={(e) => {
            setSearch(e.target.value);
            setPage(1);
          }}
        />
        <div style={{ fontSize: 13, color: "var(--sp-color-muted)" }}>
          {rows.length} results
        </div>
      </div>

      {error && (
        <div
          style={{
            padding: "10px 12px",
            borderRadius: "var(--sp-inline-radius)",
            background: "var(--sp-feedback-danger-bg)",
            border: "1px solid var(--sp-feedback-danger-border)",
            color: "var(--sp-feedback-danger-text)",
            fontSize: 13,
          }}
        >
          Failed to load overrides: {error}
        </div>
      )}

      {loading ? (
        <Table
          columns={columns}
          data={[]}
          keyExtractor={(r) => r.id}
          loading
          emptyMessage="No overrides found"
        />
      ) : hasRows ? (
        <Table
          columns={columns}
          data={rows}
          keyExtractor={(r) => r.id}
          loading={false}
          emptyMessage="No overrides found"
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
            No overrides
          </div>
          {!readOnly && (
            <button style={buttonPrimary} onClick={openCreateModal}>
              Create override
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
        title={canEditContext ? "Create Context Override" : "Create Override"}
        footer={
          <>
            <button style={buttonSecondary} onClick={() => setShowCreate(false)}>
              Cancel
            </button>
            <button
              style={buttonPrimary}
              onClick={handleCreate}
              disabled={createDisabled}
            >
              {createMutation.loading ? "Creating..." : "Create"}
            </button>
          </>
        }
      >
        <StructuredContextOverrideForm
          contextEntries={contextEntries}
          overrideEntries={overrideEntries}
          dimensions={dimensionOptions}
          defaultConfigs={defaultConfigOptions}
          lockedScope={scopedContext}
          lockedKeys={lockedDims}
          showContextFields={canEditContext}
          onAddContextKey={(key) => {
            const dimension = dimensionOptions.find((item) => item.dimension === key);
            if (!dimension) return;
            setContextEntries((current) => [
              ...current,
              defaultEntryFromSchema(key, dimension.schema),
            ]);
          }}
          onUpdateContextEntry={(key, update) =>
            updateEntryState(setContextEntries, key, update)
          }
          onRemoveContextKey={(key) =>
            setContextEntries((current) => current.filter((entry) => entry.key !== key))
          }
          onAddOverrideKey={(key) => {
            const configItem = defaultConfigOptions.find((item) => item.key === key);
            if (!configItem) return;
            setOverrideEntries((current) => [
              ...current,
              defaultEntryFromSchema(key, configItem.schema),
            ]);
          }}
          onUpdateOverrideEntry={(key, update) =>
            updateEntryState(setOverrideEntries, key, update)
          }
          onRemoveOverrideKey={(key) =>
            setOverrideEntries((current) => current.filter((entry) => entry.key !== key))
          }
        />
        {(parsedContext.error || parsedOverride.error) && (
          <div
            style={{
              marginTop: -6,
              padding: "10px 12px",
              borderRadius: "var(--sp-inline-radius)",
              background: "var(--sp-feedback-danger-bg)",
              border: "1px solid var(--sp-feedback-danger-border)",
              color: "var(--sp-feedback-danger-text)",
              fontSize: 13,
            }}
          >
            {parsedContext.error ?? parsedOverride.error}
          </div>
        )}
        <FormField label="Description">
          <input
            style={inputStyle}
            value={newDesc}
            onChange={(e) => setNewDesc(e.target.value)}
            placeholder="Enter a description"
          />
        </FormField>
        <FormField label="Reason for Change" required>
          <input
            style={inputStyle}
            value={newReason}
            onChange={(e) => setNewReason(e.target.value)}
            placeholder="Enter a reason for this change"
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
