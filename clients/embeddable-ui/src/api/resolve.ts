import type { Config, JsonValue } from "../types";
import type { SuperpositionClient } from "./client";

export function resolveApi(client: SuperpositionClient) {
  return {
    getConfig(context: Record<string, JsonValue> = {}): Promise<Config> {
      return client.get("/config", { dimension: context });
    },

    resolve(
      context: Record<string, JsonValue>,
      mergeStrategy?: "DEEP" | "SHALLOW",
    ): Promise<Record<string, JsonValue>> {
      return client.get("/config/resolve", {
        dimension: context,
        merge_strategy: mergeStrategy,
      });
    },
  };
}
