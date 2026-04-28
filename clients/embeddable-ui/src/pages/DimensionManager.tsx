import { useCallback, useMemo, useState } from "react";
import {
  buttonDanger,
  buttonPrimary,
  buttonSecondary,
  FormField,
  inputStyle,
  Modal,
  Pagination,
  Table,
} from "../components";
import type { Column } from "../components/Table";
import { useApi, useMutation } from "../hooks/useApi";
import { useAlerts } from "../providers/AlertProvider";
import { useSuperposition } from "../providers/SuperpositionUIProvider";
import type { CreateDimensionRequest, Dimension } from "../types";
import { normalizeFilterValues, paginateRows } from "../utils";
import {
  canUseFeatureAction,
  FeatureUnavailable,
  getMessage,
  isFeatureEnabled,
} from "./FeatureGate";

export interface DimensionManagerProps {
  pageSize?: number;
  /** Allow create/delete controls for dimensions. Defaults to view-only. */
  editable?: boolean;
}

function filterDimensions(rows: Dimension[], allowedDimensions?: string[]): Dimension[] {
  if (!allowedDimensions || allowedDimensions.length === 0) return rows;
  const allowed = new Set(allowedDimensions);
  return rows.filter((row) => allowed.has(row.dimension));
}

function formatDimensionType(dt: Dimension["dimension_type"]): string {
  if (typeof dt === "string") {
    return dt;
  }

  if ("REGULAR" in dt) {
    return "REGULAR";
  }

  if ("LOCAL_COHORT" in dt) {
    return `LOCAL_COHORT:${dt.LOCAL_COHORT}`;
  }

  if ("REMOTE_COHORT" in dt) {
    return `REMOTE_COHORT:${dt.REMOTE_COHORT}`;
  }

  return JSON.stringify(dt);
}

function DimensionManagerContent({
  pageSize = 20,
  editable = false,
}: DimensionManagerProps) {
  const { config, dimensions } = useSuperposition();
  const { addAlert, confirmAction } = useAlerts();
  const [page, setPage] = useState(1);
  const [showCreate, setShowCreate] = useState(false);
  const allowedDimensions = useMemo(
    () => normalizeFilterValues(config.filters?.dimensions),
    [config.filters?.dimensions],
  );
  const shouldClientPage = Boolean(allowedDimensions && allowedDimensions.length > 0);
  const canCreate = editable && canUseFeatureAction(config, "dimensions", "create");
  const canDelete = editable && canUseFeatureAction(config, "dimensions", "delete");

  const { data, loading, refetch } = useApi(
    () => dimensions.list(shouldClientPage ? { all: true } : { page, count: pageSize }),
    [dimensions, page, pageSize, shouldClientPage],
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
    const confirmed = await confirmAction({
      title: `Delete dimension "${name}"?`,
      description: "This removes the dimension definition from the workspace.",
      confirmLabel: "Delete",
      cancelLabel: "Cancel",
      variant: "destructive",
    });
    if (!confirmed) return;

    try {
      await deleteMutation.mutate(name);
      refetch();
    } catch {
      addAlert("error", deleteMutation.error || "Failed to delete");
    }
  };

  const columns: Column<Dimension>[] = [
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
      render: (row) =>
        !canDelete ? null : (
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
  ];

  const filteredRows = filterDimensions(data?.data ?? [], allowedDimensions);
  const rows = shouldClientPage
    ? paginateRows(filteredRows, page, pageSize)
    : filteredRows;
  const totalPages = shouldClientPage
    ? Math.ceil(filteredRows.length / pageSize)
    : (data?.total_pages ?? 0);

  return (
    <div>
      <div
        style={{
          display: "flex",
          justifyContent: "space-between",
          alignItems: "center",
          marginBottom: 16,
        }}
      >
        <h2
          style={{
            margin: "var(--sp-page-title-margin)",
            fontSize: "var(--sp-page-title-font-size)",
            fontWeight: "var(--sp-page-title-font-weight)",
            color: "var(--sp-page-title-text)",
          }}
        >
          Dimensions
        </h2>
        {canCreate && (
          <button style={buttonPrimary} onClick={() => setShowCreate(true)}>
            {getMessage(config, "dimensions.create", "+ Create Dimension")}
          </button>
        )}
      </div>

      <Table
        columns={columns}
        data={rows}
        keyExtractor={(r) => r.dimension}
        loading={loading}
        emptyMessage="No dimensions found"
      />

      {data && totalPages > 1 && (
        <Pagination currentPage={page} totalPages={totalPages} onPageChange={setPage} />
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
              disabled={!canCreate || createMutation.loading || !newName}
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

export function DimensionManager(props: DimensionManagerProps) {
  const { config } = useSuperposition();

  if (!isFeatureEnabled(config.features, "dimensions")) {
    return (
      <FeatureUnavailable
        feature="Dimensions"
        message={getMessage(
          config,
          "feature.disabled",
          "{feature} is not enabled for this embed.",
          { feature: "Dimensions" },
        )}
      />
    );
  }

  return <DimensionManagerContent {...props} />;
}
