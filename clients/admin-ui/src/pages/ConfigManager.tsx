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
  JsonViewer,
} from "../components";
import type { Column } from "../components/Table";
import type {
  DefaultConfig,
  CreateDefaultConfigRequest,
} from "../types";

export interface ConfigManagerProps {
  /** Items per page */
  pageSize?: number;
}

export function ConfigManager({ pageSize = 20 }: ConfigManagerProps) {
  const { defaultConfigs } = useSuperposition();
  const { addAlert } = useAlerts();
  const [page, setPage] = useState(1);
  const [search, setSearch] = useState("");
  const [showCreate, setShowCreate] = useState(false);

  const { data, loading, refetch } = useApi(
    () => defaultConfigs.list({ page, count: pageSize }, { name: search || undefined }),
    [page, pageSize, search],
  );

  // Create form state
  const [newKey, setNewKey] = useState("");
  const [newValue, setNewValue] = useState("null");
  const [newSchema, setNewSchema] = useState('{"type": "string"}');
  const [newDesc, setNewDesc] = useState("");
  const [newReason, setNewReason] = useState("");

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
    try {
      await createMutation.mutate({
        key: newKey,
        value: JSON.parse(newValue),
        schema: JSON.parse(newSchema),
        description: newDesc || "No description",
        change_reason: newReason || "Created via admin UI",
      });
      setShowCreate(false);
      setNewKey("");
      setNewValue("null");
      setNewSchema('{"type": "string"}');
      setNewDesc("");
      setNewReason("");
      refetch();
    } catch {
      addAlert("error", createMutation.error || "Failed to create config");
    }
  };

  const handleDelete = async (key: string) => {
    if (!confirm(`Delete config "${key}"?`)) return;
    try {
      await deleteMutation.mutate(key);
      refetch();
    } catch {
      addAlert("error", deleteMutation.error || "Failed to delete config");
    }
  };

  const columns: Column<DefaultConfig>[] = useMemo(
    () => [
      { key: "key", header: "Key", width: "25%" },
      {
        key: "value",
        header: "Value",
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
        render: (row) => (
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
    ],
    // eslint-disable-next-line react-hooks/exhaustive-deps
    [],
  );

  return (
    <div>
      <div style={{ display: "flex", justifyContent: "space-between", alignItems: "center", marginBottom: 16 }}>
        <h2 style={{ margin: 0, fontSize: 20, fontWeight: 600 }}>Default Configs</h2>
        <button style={buttonPrimary} onClick={() => setShowCreate(true)}>
          + Create Config
        </button>
      </div>

      <div style={{ marginBottom: 12 }}>
        <input
          style={{ ...inputStyle, maxWidth: 300 }}
          placeholder="Search by key..."
          value={search}
          onChange={(e) => {
            setSearch(e.target.value);
            setPage(1);
          }}
        />
      </div>

      <Table
        columns={columns}
        data={data?.data ?? []}
        keyExtractor={(r) => r.key}
        loading={loading}
        emptyMessage="No configs found"
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
        title="Create Default Config"
        footer={
          <>
            <button style={buttonSecondary} onClick={() => setShowCreate(false)}>
              Cancel
            </button>
            <button
              style={buttonPrimary}
              onClick={handleCreate}
              disabled={createMutation.loading || !newKey}
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
        <FormField label="Value (JSON)" required>
          <textarea
            style={{ ...inputStyle, fontFamily: "monospace", minHeight: 60 }}
            value={newValue}
            onChange={(e) => setNewValue(e.target.value)}
          />
        </FormField>
        <FormField label="Schema (JSON)" required>
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
      </Modal>
    </div>
  );
}
