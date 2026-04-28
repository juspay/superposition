import type { SuperpositionClient } from "./client";
import type {
  ConcludeExperimentRequest,
  CreateExperimentRequest,
  Experiment,
  ExperimentListFilters,
  PaginatedResponse,
  PaginationParams,
  RampExperimentRequest,
} from "../types";

export function experimentsApi(client: SuperpositionClient) {
  return {
    list(
      params: PaginationParams = {},
      filters: ExperimentListFilters = {},
    ): Promise<PaginatedResponse<Experiment>> {
      return client.get("/experiments", {
        ...params,
        status: filters.status,
        from_date: filters.from_date,
        to_date: filters.to_date,
        experiment_name: filters.experiment_name,
        experiment_ids: filters.experiment_ids,
        experiment_group_ids: filters.experiment_group_ids,
        created_by: filters.created_by,
        sort_on: filters.sort_on,
        sort_by: filters.sort_by,
      });
    },

    get(id: string): Promise<Experiment> {
      return client.get(`/experiments/${encodeURIComponent(id)}`);
    },

    create(req: CreateExperimentRequest): Promise<Experiment> {
      return client.post("/experiments", req);
    },

    ramp(id: string, req: RampExperimentRequest): Promise<Experiment> {
      return client.patch(`/experiments/${encodeURIComponent(id)}/ramp`, req);
    },

    conclude(id: string, req: ConcludeExperimentRequest): Promise<Experiment> {
      return client.patch(`/experiments/${encodeURIComponent(id)}/conclude`, req);
    },

    pause(id: string, changeReason: string): Promise<Experiment> {
      return client.patch(`/experiments/${encodeURIComponent(id)}/pause`, {
        change_reason: changeReason,
      });
    },

    resume(id: string, changeReason: string): Promise<Experiment> {
      return client.patch(`/experiments/${encodeURIComponent(id)}/resume`, {
        change_reason: changeReason,
      });
    },

    discard(id: string, changeReason: string): Promise<Experiment> {
      return client.patch(`/experiments/${encodeURIComponent(id)}/discard`, {
        change_reason: changeReason,
      });
    },
  };
}
