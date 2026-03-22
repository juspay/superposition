import type { SuperpositionClient } from "./client";
import type {
  CreateDefaultConfigRequest,
  DefaultConfig,
  DefaultConfigFilters,
  PaginatedResponse,
  PaginationParams,
  UpdateDefaultConfigRequest,
} from "../types";

export function defaultConfigsApi(client: SuperpositionClient) {
  return {
    list(
      params: PaginationParams = {},
      filters: DefaultConfigFilters = {},
    ): Promise<PaginatedResponse<DefaultConfig>> {
      return client.get("/default-config", { ...params, ...filters });
    },

    get(key: string): Promise<DefaultConfig> {
      return client.get(`/default-config/${encodeURIComponent(key)}`);
    },

    create(req: CreateDefaultConfigRequest): Promise<DefaultConfig> {
      return client.post("/default-config", req);
    },

    update(key: string, req: UpdateDefaultConfigRequest): Promise<DefaultConfig> {
      return client.patch(`/default-config/${encodeURIComponent(key)}`, req);
    },

    delete(key: string): Promise<void> {
      return client.delete(`/default-config/${encodeURIComponent(key)}`);
    },
  };
}
