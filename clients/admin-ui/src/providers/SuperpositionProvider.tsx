import React, { createContext, useContext, useMemo } from "react";
import { SuperpositionClient } from "../api/client";
import { dimensionsApi } from "../api/dimensions";
import { defaultConfigsApi } from "../api/default-configs";
import { overridesApi } from "../api/overrides";
import { experimentsApi } from "../api/experiments";
import { resolveApi } from "../api/resolve";
import { auditApi } from "../api/audit";
import type { SuperpositionConfig } from "../types";

export interface SuperpositionContextValue {
  config: SuperpositionConfig;
  client: SuperpositionClient;
  dimensions: ReturnType<typeof dimensionsApi>;
  defaultConfigs: ReturnType<typeof defaultConfigsApi>;
  overrides: ReturnType<typeof overridesApi>;
  experiments: ReturnType<typeof experimentsApi>;
  resolve: ReturnType<typeof resolveApi>;
  audit: ReturnType<typeof auditApi>;
}

const SuperpositionContext = createContext<SuperpositionContextValue | null>(null);

export function useSuperposition(): SuperpositionContextValue {
  const ctx = useContext(SuperpositionContext);
  if (!ctx) {
    throw new Error("useSuperposition must be used within a <SuperpositionProvider>");
  }
  return ctx;
}

export interface SuperpositionProviderProps {
  config: SuperpositionConfig;
  children: React.ReactNode;
}

export function SuperpositionProvider({
  config,
  children,
}: SuperpositionProviderProps) {
  const value = useMemo<SuperpositionContextValue>(() => {
    const client = new SuperpositionClient({
      host: config.host,
      orgId: config.orgId,
      workspace: config.workspace,
      auth: config.auth,
    });

    return {
      config,
      client,
      dimensions: dimensionsApi(client),
      defaultConfigs: defaultConfigsApi(client),
      overrides: overridesApi(client),
      experiments: experimentsApi(client),
      resolve: resolveApi(client),
      audit: auditApi(client),
    };
  }, [config]);

  return (
    <SuperpositionContext.Provider value={value}>
      {children}
    </SuperpositionContext.Provider>
  );
}
