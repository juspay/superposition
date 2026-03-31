import type { SuperpositionClient } from "./client";
import type {
  ContextListFilters,
  ContextOverride,
  PaginatedResponse,
  PaginationParams,
  PutContextRequest,
} from "../types";

export function overridesApi(client: SuperpositionClient) {
  return {
    list(
      params: PaginationParams = {},
      filters: ContextListFilters = {},
    ): Promise<PaginatedResponse<ContextOverride>> {
      return client.get("/context", {
        ...params,
        prefix: filters.prefix,
        sort_on: filters.sort_on,
        sort_by: filters.sort_by,
        created_by: filters.created_by,
        last_modified_by: filters.last_modified_by,
        plaintext: filters.plaintext,
      });
    },

    get(id: string): Promise<ContextOverride> {
      return client.get(`/context/${encodeURIComponent(id)}`);
    },

    create(req: PutContextRequest): Promise<ContextOverride> {
      return client.put("/context/put", {
        context: req.context,
        override: req.override,
        description: req.description,
        change_reason: req.change_reason,
      });
    },

    delete(id: string): Promise<void> {
      return client.delete(`/context/${encodeURIComponent(id)}`);
    },

    bulkOperations(operations: unknown[]): Promise<unknown> {
      return client.put("/context/bulk-operations", { operations });
    },
  };
}
