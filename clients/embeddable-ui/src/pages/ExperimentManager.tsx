import { useCallback, useMemo, useState } from "react";
import {
  buttonPrimary,
  buttonSecondary,
  ConditionBadges,
  FormField,
  inputStyle,
  Modal,
  Pagination,
  ScopedContextEditor,
  StatusBadge,
  Table,
} from "../components";
import type { Column } from "../components/Table";
import { useApi, useMutation } from "../hooks/useApi";
import { useAlerts } from "../providers/AlertProvider";
import { useSuperposition } from "../providers/SuperpositionProvider";
import type { CreateExperimentRequest, Experiment, ExperimentStatus } from "../types";
import {
  filterExperimentsByScope,
  mergeScopedContext,
  stripScopedDimensions,
} from "../utils/context-filter";
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
  const { experiments, config, scope } = useSuperposition();
  const { addAlert } = useAlerts();
  const [page, setPage] = useState(1);
  const [statusFilter, setStatusFilter] = useState<ExperimentStatus[]>([]);
  const [showCreate, setShowCreate] = useState(false);

  const scopedContext = scope.effectiveContext;
  const readOnly = config.readOnly === true;
  const lockedDims = scope.lockedDimensions;

  const { data, loading, error, refetch } = useApi(
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

  const parsedContext = useMemo(() => {
    try {
      const value = JSON.parse(newContext);
      if (!value || Array.isArray(value) || typeof value !== "object") {
        return { value: null, error: "Context must be a JSON object." };
      }

      return { value: value as CreateExperimentRequest["context"], error: null };
    } catch {
      return {
        value: null,
        error: 'Enter valid context JSON, for example {"dimension":"value"}.',
      };
    }
  }, [newContext]);

  const parsedVariants = useMemo(() => {
    try {
      const value = JSON.parse(newVariants);
      if (!Array.isArray(value)) {
        return { value: null, error: "Variants must be a JSON array." };
      }

      return {
        value: value as CreateExperimentRequest["variants"],
        error: null,
      };
    } catch {
      return {
        value: null,
        error: "Enter valid variant JSON before creating the experiment.",
      };
    }
  }, [newVariants]);

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
    if (parsedContext.error) {
      addAlert("error", parsedContext.error);
      return;
    }

    if (parsedVariants.error) {
      addAlert("error", parsedVariants.error);
      return;
    }

    try {
      const { context: additionalContext, removedKeys } = stripScopedDimensions(
        parsedContext.value as CreateExperimentRequest["context"],
        scopedContext,
      );
      const ctx = mergeScopedContext(additionalContext, scopedContext);

      await createMutation.mutate({
        name: newName,
        context: ctx,
        variants: parsedVariants.value as CreateExperimentRequest["variants"],
        description: newDesc || "No description",
        change_reason: newReason || "Created via admin UI",
      });

      if (removedKeys.length > 0) {
        addAlert(
          "info",
          `Locked scope applied automatically for: ${removedKeys.join(", ")}`,
        );
      }

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

  const columns: Column<Experiment>[] = [
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
        !readOnly && row.status === "INPROGRESS" ? (
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
  ];

  const createDisabled =
    readOnly ||
    createMutation.loading ||
    !newName.trim() ||
    !newReason.trim() ||
    !!parsedContext.error ||
    !!parsedVariants.error;

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
        <h2 style={{ margin: 0, fontSize: 20, fontWeight: 600 }}>Experiments</h2>
        {!readOnly && (
          <button style={buttonPrimary} onClick={() => setShowCreate(true)}>
            + Create Experiment
          </button>
        )}
      </div>

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
          Failed to load experiments: {error}
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
              background: statusFilter.includes(s)
                ? "var(--sp-feedback-info-bg)"
                : "var(--sp-button-secondary-bg)",
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
              disabled={createDisabled}
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
        <ScopedContextEditor
          value={newContext}
          onChange={setNewContext}
          scopedContext={scopedContext}
          lockedKeys={lockedDims}
          error={parsedContext.error ?? undefined}
          minHeight={60}
        />
        <FormField
          label="Variants (JSON array)"
          required
          error={parsedVariants.error ?? undefined}
        >
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
