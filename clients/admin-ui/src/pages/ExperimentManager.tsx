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
  ConditionBadges,
  StatusBadge,
} from "../components";
import type { Column } from "../components/Table";
import type {
  Experiment,
  ExperimentStatus,
  CreateExperimentRequest,
} from "../types";
import { filterExperimentsByScope, getLockedDimensions } from "../utils/context-filter";
import { formatDateTime } from "../utils/format";

export interface ExperimentManagerProps {
  pageSize?: number;
}

const statusOptions: ExperimentStatus[] = [
  "CREATED",
  "INPROGRESS",
  "PAUSED",
  "CONCLUDED",
  "DISCARDED",
];

export function ExperimentManager({ pageSize = 20 }: ExperimentManagerProps) {
  const { experiments, config } = useSuperposition();
  const { addAlert } = useAlerts();
  const [page, setPage] = useState(1);
  const [statusFilter, setStatusFilter] = useState<ExperimentStatus[]>([]);
  const [showCreate, setShowCreate] = useState(false);

  const scopedContext = config.context;
  const lockedDims = getLockedDimensions(scopedContext);

  const { data, loading, refetch } = useApi(
    () =>
      experiments.list(
        { page, count: pageSize },
        { status: statusFilter.length > 0 ? statusFilter : undefined },
      ),
    [page, pageSize, statusFilter],
  );

  const filteredData = useMemo(() => {
    if (!data) return [];
    return filterExperimentsByScope(data.data, scopedContext);
  }, [data, scopedContext]);

  // Create form
  const [newName, setNewName] = useState("");
  const [newContext, setNewContext] = useState("{}");
  const [newVariants, setNewVariants] = useState(
    JSON.stringify(
      [
        { id: "control", variant_type: "CONTROL", overrides: {} },
        { id: "variant-1", variant_type: "EXPERIMENTAL", overrides: {} },
      ],
      null,
      2,
    ),
  );
  const [newDesc, setNewDesc] = useState("");
  const [newReason, setNewReason] = useState("");

  const createMutation = useMutation(
    useCallback(
      async (req: CreateExperimentRequest) => {
        const result = await experiments.create(req);
        addAlert("success", `Experiment "${result.name}" created`);
        return result;
      },
      [experiments, addAlert],
    ),
  );

  const handleCreate = async () => {
    try {
      const ctx = scopedContext
        ? { ...JSON.parse(newContext), ...scopedContext }
        : JSON.parse(newContext);
      await createMutation.mutate({
        name: newName,
        context: ctx,
        variants: JSON.parse(newVariants),
        description: newDesc || "No description",
        change_reason: newReason || "Created via admin UI",
      });
      setShowCreate(false);
      setNewName("");
      setNewContext("{}");
      setNewDesc("");
      setNewReason("");
      refetch();
    } catch {
      addAlert("error", createMutation.error || "Failed to create experiment");
    }
  };

  // Quick actions
  const rampMutation = useMutation(
    useCallback(
      async (id: string, pct: number) => {
        await experiments.ramp(id, {
          traffic_percentage: pct,
          change_reason: "Ramp change via admin UI",
        });
        addAlert("success", `Experiment ramped to ${pct}%`);
      },
      [experiments, addAlert],
    ),
  );

  const handleRamp = async (id: string) => {
    const input = prompt("Enter traffic percentage (0-100):");
    if (input === null) return;
    const pct = parseInt(input, 10);
    if (isNaN(pct) || pct < 0 || pct > 100) {
      addAlert("error", "Invalid percentage");
      return;
    }
    try {
      await rampMutation.mutate(id, pct);
      refetch();
    } catch {
      addAlert("error", rampMutation.error || "Failed to ramp");
    }
  };

  const columns: Column<Experiment>[] = useMemo(
    () => [
      { key: "name", header: "Name", width: "20%" },
      {
        key: "status",
        header: "Status",
        width: "12%",
        render: (row) => <StatusBadge status={row.status} />,
      },
      {
        key: "traffic_percentage",
        header: "Traffic",
        width: "8%",
        render: (row) => `${row.traffic_percentage}%`,
      },
      {
        key: "context",
        header: "Context",
        width: "25%",
        render: (row) => (
          <ConditionBadges condition={row.context} lockedKeys={lockedDims} />
        ),
      },
      {
        key: "variants",
        header: "Variants",
        width: "10%",
        render: (row) => `${row.variants.length}`,
      },
      {
        key: "created_at",
        header: "Created",
        width: "15%",
        render: (row) => formatDateTime(row.created_at),
      },
      {
        key: "actions",
        header: "",
        width: "10%",
        render: (row) =>
          row.status === "INPROGRESS" ? (
            <button
              style={{ ...buttonSecondary, fontSize: 12, padding: "4px 8px" }}
              onClick={(e) => {
                e.stopPropagation();
                handleRamp(row.id);
              }}
            >
              Ramp
            </button>
          ) : null,
      },
    ],
    // eslint-disable-next-line react-hooks/exhaustive-deps
    [lockedDims],
  );

  return (
    <div>
      <div style={{ display: "flex", justifyContent: "space-between", alignItems: "center", marginBottom: 16 }}>
        <h2 style={{ margin: 0, fontSize: 20, fontWeight: 600 }}>Experiments</h2>
        <button style={buttonPrimary} onClick={() => setShowCreate(true)}>
          + Create Experiment
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

      <div style={{ marginBottom: 12, display: "flex", gap: 6, flexWrap: "wrap" }}>
        {statusOptions.map((s) => (
          <button
            key={s}
            style={{
              ...buttonSecondary,
              fontSize: 12,
              padding: "4px 10px",
              background: statusFilter.includes(s) ? "#e0e7ff" : "#fff",
            }}
            onClick={() =>
              setStatusFilter((prev) =>
                prev.includes(s) ? prev.filter((x) => x !== s) : [...prev, s],
              )
            }
          >
            {s}
          </button>
        ))}
      </div>

      <Table
        columns={columns}
        data={filteredData}
        keyExtractor={(r) => r.id}
        loading={loading}
        emptyMessage="No experiments found"
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
        title="Create Experiment"
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
            placeholder="experiment-name"
          />
        </FormField>
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
            style={{ ...inputStyle, fontFamily: "monospace", minHeight: 60 }}
            value={newContext}
            onChange={(e) => setNewContext(e.target.value)}
          />
        </FormField>
        <FormField label="Variants (JSON array)" required>
          <textarea
            style={{ ...inputStyle, fontFamily: "monospace", minHeight: 120 }}
            value={newVariants}
            onChange={(e) => setNewVariants(e.target.value)}
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
