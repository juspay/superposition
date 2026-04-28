import type {
  CreateDefaultConfigRequest,
  DefaultConfig,
  DefaultConfigFilters,
  PaginatedResponse,
  PaginationParams,
  UpdateDefaultConfigRequest,
} from "../types";
import type { SuperpositionClient } from "./client";
import { SuperpositionApiError } from "./client";

export function defaultConfigsApi(client: SuperpositionClient) {
  return {
    list(
      params: PaginationParams = {},
      filters: DefaultConfigFilters = {},
    ): Promise<PaginatedResponse<DefaultConfig>> {
      return client
        .get<PaginatedResponse<DefaultConfig>>("/default-config", {
          ...params,
          name: filters.name,
          prefix: filters.prefix,
        })
        .catch((error) => {
          if (
            error instanceof SuperpositionApiError &&
            error.status === 404 &&
            error.body.includes("No records found")
          ) {
            return {
              total_pages: 0,
              total_items: 0,
              data: [],
            } satisfies PaginatedResponse<DefaultConfig>;
          }

          throw error;
        });
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
