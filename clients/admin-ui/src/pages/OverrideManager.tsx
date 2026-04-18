import { useCallback, useMemo, useState } from "react";
import { useSuperposition } from "../providers/SuperpositionProvider";
import { useAlerts } from "../providers/AlertProvider";
import { useApi, useMutation } from "../hooks/useApi";
import {
  Table,
  Pagination,
  Modal,
  FormField,
  inputStyle,
  buttonPrimary,
  buttonSecondary,
  buttonDanger,
  ConditionBadges,
  JsonViewer,
} from "../components";
import type { Column } from "../components/Table";
import type { ContextOverride, PutContextRequest } from "../types";
import {
  filterOverridesByScope,
  getLockedDimensions,
  mergeScopedContext,
} from "../utils/context-filter";

export interface OverrideManagerProps {
  pageSize?: number;
}

export function OverrideManager({ pageSize = 20 }: OverrideManagerProps) {
  const { overrides, config } = useSuperposition();
  const { addAlert } = useAlerts();
  const [page, setPage] = useState(1);
  const [search, setSearch] = useState("");
  const [showCreate, setShowCreate] = useState(false);

  const scopedContext = config.context;
  const lockDimensions = config.lockScopedDimensions !== false;
  const lockedDims = lockDimensions ? getLockedDimensions(scopedContext) : [];

  const { data, loading, refetch } = useApi(
    () => overrides.list({ page, count: pageSize }, { plaintext: search || undefined }),
    [page, pageSize, search],
  );

  // Apply client-side context scoping filter
  const filteredData = useMemo(() => {
    if (!data) return [];
    return filterOverridesByScope(data.data, scopedContext);
  }, [data, scopedContext]);

  // Create form
  const [newContext, setNewContext] = useState("{}");
  const [newOverride, setNewOverride] = useState("{}");
  const [newDesc, setNewDesc] = useState("");
  const [newReason, setNewReason] = useState("");

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
    try {
      const ctx = mergeScopedContext(JSON.parse(newContext), scopedContext);
      await createMutation.mutate({
        context: ctx,
        override: JSON.parse(newOverride),
        description: newDesc || undefined,
        change_reason: newReason || "Created via admin UI",
      });
      setShowCreate(false);
      setNewContext("{}");
      setNewOverride("{}");
      setNewDesc("");
      setNewReason("");
      refetch();
    } catch {
      addAlert("error", createMutation.error || "Failed to create override");
    }
  };

  const handleDelete = async (id: string) => {
    if (!confirm("Delete this override?")) return;
    try {
      await deleteMutation.mutate(id);
      refetch();
    } catch {
      addAlert("error", deleteMutation.error || "Failed to delete");
    }
  };

  const columns: Column<ContextOverride>[] = useMemo(
    () => [
      {
        key: "value",
        header: "Context",
        width: "35%",
        render: (row) => (
          <ConditionBadges condition={row.value} lockedKeys={lockedDims} />
        ),
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
        render: (row) => (
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
    ],
    // eslint-disable-next-line react-hooks/exhaustive-deps
    [lockedDims],
  );

  return (
    <div>
      <div style={{ display: "flex", justifyContent: "space-between", alignItems: "center", marginBottom: 16 }}>
        <h2 style={{ margin: 0, fontSize: 20, fontWeight: 600 }}>Context Overrides</h2>
        <button style={buttonPrimary} onClick={() => setShowCreate(true)}>
          + Create Override
        </button>
      </div>

      {scopedContext && Object.keys(scopedContext).length > 0 && (
        <div style={{ marginBottom: 12, padding: "8px 12px", background: "#fef3c7", borderRadius: 6, fontSize: 13 }}>
          Scoped to:{" "}
          <ConditionBadges
            condition={scopedContext!}
            lockedKeys={lockedDims}
          />
        </div>
      )}

      <div style={{ marginBottom: 12 }}>
        <input
          style={{ ...inputStyle, maxWidth: 300 }}
          placeholder="Search overrides..."
          value={search}
          onChange={(e) => {
            setSearch(e.target.value);
            setPage(1);
          }}
        />
      </div>

      <Table
        columns={columns}
        data={filteredData}
        keyExtractor={(r) => r.id}
        loading={loading}
        emptyMessage="No overrides found"
      />

      {data && (
        <Pagination
          currentPage={page}
          totalPages={data.total_pages}
          onPageChange={setPage}
        />
      )}

      <Modal
        open={showCreate}
        onClose={() => setShowCreate(false)}
        title="Create Context Override"
        footer={
          <>
            <button style={buttonSecondary} onClick={() => setShowCreate(false)}>
              Cancel
            </button>
            <button
              style={buttonPrimary}
              onClick={handleCreate}
              disabled={createMutation.loading}
            >
              {createMutation.loading ? "Creating..." : "Create"}
            </button>
          </>
        }
      >
        {scopedContext && Object.keys(scopedContext).length > 0 && (
          <div style={{ marginBottom: 12, padding: "8px 12px", background: "#f3f4f6", borderRadius: 4, fontSize: 13 }}>
            Scoped dimensions (auto-applied):
            <ConditionBadges
              condition={scopedContext!}
              lockedKeys={lockedDims}
            />
          </div>
        )}
        <FormField label="Additional Context (JSON)">
          <textarea
            style={{ ...inputStyle, fontFamily: "monospace", minHeight: 80 }}
            value={newContext}
            onChange={(e) => setNewContext(e.target.value)}
            placeholder='{"dimension": "value"}'
          />
        </FormField>
        <FormField label="Override Values (JSON)" required>
          <textarea
            style={{ ...inputStyle, fontFamily: "monospace", minHeight: 80 }}
            value={newOverride}
            onChange={(e) => setNewOverride(e.target.value)}
            placeholder='{"config_key": "value"}'
          />
        </FormField>
        <FormField label="Description">
          <input
            style={inputStyle}
            value={newDesc}
            onChange={(e) => setNewDesc(e.target.value)}
          />
        </FormField>
        <FormField label="Change Reason" required>
          <input
            style={inputStyle}
            value={newReason}
            onChange={(e) => setNewReason(e.target.value)}
          />
        </FormField>
      </Modal>
    </div>
  );
}
