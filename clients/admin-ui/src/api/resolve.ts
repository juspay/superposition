import type { SuperpositionClient } from "./client";
import type { Config, JsonValue } from "../types";

export function resolveApi(client: SuperpositionClient) {
  return {
    getConfig(
      context: Record<string, JsonValue> = {},
    ): Promise<Config> {
      return client.get("/config", context);
    },

    resolve(
      context: Record<string, JsonValue>,
      mergeStrategy?: "DEEP" | "SHALLOW",
    ): Promise<Record<string, JsonValue>> {
      return client.post("/config/resolve", {
        context,
        merge_strategy: mergeStrategy,
      });
    },
  };
}
