import type {
  ContextListFilters,
  ContextOverride,
  PaginatedResponse,
  PaginationParams,
  PutContextRequest,
} from "../types";
import type { SuperpositionClient } from "./client";

function normalizeContextOverride(
  value: ContextOverride & { override?: ContextOverride["override_"] },
): ContextOverride {
  return {
    ...value,
    override_: value.override_ ?? value.override ?? {},
  };
}

function normalizeContextOverrideList(
  response: PaginatedResponse<
    ContextOverride & { override?: ContextOverride["override_"] }
  >,
): PaginatedResponse<ContextOverride> {
  return {
    ...response,
    data: response.data.map(normalizeContextOverride),
  };
}

export function overridesApi(client: SuperpositionClient) {
  return {
    list(
      params: PaginationParams = {},
      filters: ContextListFilters = {},
    ): Promise<PaginatedResponse<ContextOverride>> {
      return client
        .get<
          PaginatedResponse<ContextOverride & { override?: ContextOverride["override_"] }>
        >("/context", {
          ...params,
          prefix: filters.prefix,
          sort_on: filters.sort_on,
          sort_by: filters.sort_by,
          created_by: filters.created_by,
          last_modified_by: filters.last_modified_by,
          plaintext: filters.plaintext,
        })
        .then(normalizeContextOverrideList);
    },

    get(id: string): Promise<ContextOverride> {
      return client
        .get<
          ContextOverride & { override?: ContextOverride["override_"] }
        >(`/context/${encodeURIComponent(id)}`)
        .then(normalizeContextOverride);
    },

    create(req: PutContextRequest): Promise<ContextOverride> {
      return client
        .put<ContextOverride & { override?: ContextOverride["override_"] }>("/context", {
          context: req.context,
          override: req.override,
          description: req.description,
          change_reason: req.change_reason,
        })
        .then(normalizeContextOverride);
    },

    delete(id: string): Promise<void> {
      return client.delete(`/context/${encodeURIComponent(id)}`);
    },

    bulkOperations(operations: unknown[]): Promise<unknown> {
      return client.put("/context/bulk-operations", { operations });
    },
  };
}
