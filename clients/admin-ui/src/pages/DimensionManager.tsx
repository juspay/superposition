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
} from "../components";
import type { Column } from "../components/Table";
import type { Dimension, CreateDimensionRequest } from "../types";

export interface DimensionManagerProps {
  pageSize?: number;
}

function formatDimensionType(dt: Dimension["dimension_type"]): string {
  if (typeof dt === "string") return dt;
  if ("LOCAL_COHORT" in dt) return `Local Cohort (${dt.LOCAL_COHORT})`;
  if ("REMOTE_COHORT" in dt) return `Remote Cohort (${dt.REMOTE_COHORT})`;
  return String(dt);
}

export function DimensionManager({ pageSize = 20 }: DimensionManagerProps) {
  const { dimensions } = useSuperposition();
  const { addAlert } = useAlerts();
  const [page, setPage] = useState(1);
  const [showCreate, setShowCreate] = useState(false);

  const { data, loading, refetch } = useApi(
    () => dimensions.list({ page, count: pageSize }),
    [page, pageSize],
  );

  const [newName, setNewName] = useState("");
  const [newPosition, setNewPosition] = useState("0");
  const [newSchema, setNewSchema] = useState('{"type": "string"}');
  const [newDesc, setNewDesc] = useState("");
  const [newReason, setNewReason] = useState("");

  const createMutation = useMutation(
    useCallback(
      async (req: CreateDimensionRequest) => {
        const result = await dimensions.create(req);
        addAlert("success", `Dimension "${result.dimension}" created`);
        return result;
      },
      [dimensions, addAlert],
    ),
  );

  const deleteMutation = useMutation(
    useCallback(
      async (name: string) => {
        await dimensions.delete(name);
        addAlert("success", `Dimension "${name}" deleted`);
      },
      [dimensions, addAlert],
    ),
  );

  const handleCreate = async () => {
    try {
      await createMutation.mutate({
        dimension: newName,
        position: parseInt(newPosition, 10),
        schema: JSON.parse(newSchema),
        description: newDesc || "No description",
        change_reason: newReason || "Created via admin UI",
      });
      setShowCreate(false);
      setNewName("");
      setNewPosition("0");
      setNewSchema('{"type": "string"}');
      setNewDesc("");
      setNewReason("");
      refetch();
    } catch {
      addAlert("error", createMutation.error || "Failed to create dimension");
    }
  };

  const handleDelete = async (name: string) => {
    if (!confirm(`Delete dimension "${name}"?`)) return;
    try {
      await deleteMutation.mutate(name);
      refetch();
    } catch {
      addAlert("error", deleteMutation.error || "Failed to delete");
    }
  };

  const columns: Column<Dimension>[] = useMemo(
    () => [
      { key: "dimension", header: "Name", width: "20%" },
      { key: "position", header: "Position", width: "10%" },
      {
        key: "dimension_type",
        header: "Type",
        width: "15%",
        render: (row) => formatDimensionType(row.dimension_type),
      },
      { key: "description", header: "Description", width: "25%" },
      {
        key: "mandatory",
        header: "Mandatory",
        width: "10%",
        render: (row) => (row.mandatory ? "Yes" : "No"),
      },
      {
        key: "actions",
        header: "",
        width: "10%",
        render: (row) => (
          <button
            style={buttonDanger}
            onClick={(e) => {
              e.stopPropagation();
              handleDelete(row.dimension);
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
        <h2 style={{ margin: 0, fontSize: 20, fontWeight: 600 }}>Dimensions</h2>
        <button style={buttonPrimary} onClick={() => setShowCreate(true)}>
          + Create Dimension
        </button>
      </div>

      <Table
        columns={columns}
        data={data?.data ?? []}
        keyExtractor={(r) => r.dimension}
        loading={loading}
        emptyMessage="No dimensions found"
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
        title="Create Dimension"
        footer={
          <>
            <button style={buttonSecondary} onClick={() => setShowCreate(false)}>
              Cancel
            </button>
            <button
              style={buttonPrimary}
              onClick={handleCreate}
              disabled={createMutation.loading || !newName}
            >
              {createMutation.loading ? "Creating..." : "Create"}
            </button>
          </>
        }
      >
        <FormField label="Name" required>
          <input
            style={inputStyle}
            value={newName}
            onChange={(e) => setNewName(e.target.value)}
            placeholder="dimension_name"
          />
        </FormField>
        <FormField label="Position" required>
          <input
            style={inputStyle}
            type="number"
            value={newPosition}
            onChange={(e) => setNewPosition(e.target.value)}
            min={0}
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
