import type { SuperpositionClient } from "./client";
import type {
  CreateDimensionRequest,
  Dimension,
  PaginatedResponse,
  PaginationParams,
  UpdateDimensionRequest,
} from "../types";

export function dimensionsApi(client: SuperpositionClient) {
  return {
    list(params: PaginationParams = {}): Promise<PaginatedResponse<Dimension>> {
      return client.get("/dimension", { ...params });
    },

    get(name: string): Promise<Dimension> {
      return client.get(`/dimension/${encodeURIComponent(name)}`);
    },

    create(req: CreateDimensionRequest): Promise<Dimension> {
      return client.post("/dimension", req);
    },

    update(name: string, req: UpdateDimensionRequest): Promise<Dimension> {
      return client.patch(`/dimension/${encodeURIComponent(name)}`, req);
    },

    delete(name: string): Promise<void> {
      return client.delete(`/dimension/${encodeURIComponent(name)}`);
    },
  };
}
