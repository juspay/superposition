import type { SuperpositionClient } from "./client";
import type {
  AuditLogEntry,
  AuditLogFilters,
  PaginatedResponse,
  PaginationParams,
} from "../types";

export function auditApi(client: SuperpositionClient) {
  return {
    list(
      params: PaginationParams = {},
      filters: AuditLogFilters = {},
    ): Promise<PaginatedResponse<AuditLogEntry>> {
      return client.get("/audit", {
        ...params,
        from_date: filters.from_date,
        to_date: filters.to_date,
        table: filters.table,
        action: filters.action,
        username: filters.username,
        sort_by: filters.sort_by,
      });
    },
  };
}
