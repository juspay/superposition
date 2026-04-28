import React, { createContext, useCallback, useContext, useMemo, useState } from "react";
import { auditApi } from "../api/audit";
import { SuperpositionClient } from "../api/client";
import { defaultConfigsApi } from "../api/default-configs";
import { dimensionsApi } from "../api/dimensions";
import { experimentsApi } from "../api/experiments";
import { overridesApi } from "../api/overrides";
import { resolveApi } from "../api/resolve";
import type { JsonValue, SuperpositionEmbeddableConfig } from "../types";
import { getLockedDimensions, mergeScopedContext } from "../utils/context-filter";
import { normalizeSuperpositionConfig } from "../utils/normalize-config";
import { ThemeProvider } from "./ThemeProvider";

export type BoundaryContext = Record<string, JsonValue>;

export interface SuperpositionScopeState {
  hostContext?: BoundaryContext;
  boundaryContext?: BoundaryContext;
  effectiveContext?: BoundaryContext;
  lockedDimensions: string[];
  hasBoundaryContext: boolean;
  setBoundaryContext: (context?: BoundaryContext) => void;
  clearBoundaryContext: () => void;
}

export interface SuperpositionContextValue {
  config: SuperpositionEmbeddableConfig;
  client: SuperpositionClient;
  dimensions: ReturnType<typeof dimensionsApi>;
  defaultConfigs: ReturnType<typeof defaultConfigsApi>;
  overrides: ReturnType<typeof overridesApi>;
  experiments: ReturnType<typeof experimentsApi>;
  resolve: ReturnType<typeof resolveApi>;
  audit: ReturnType<typeof auditApi>;
  scope: SuperpositionScopeState;
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
  config: SuperpositionEmbeddableConfig;
  children: React.ReactNode;
}

export function SuperpositionProvider({ config, children }: SuperpositionProviderProps) {
  const normalizedConfig = useMemo(() => normalizeSuperpositionConfig(config), [config]);
  const [boundaryContext, setBoundaryContextState] = useState<
    BoundaryContext | undefined
  >();

  const setBoundaryContext = useCallback((next?: BoundaryContext) => {
    if (!next || Object.keys(next).length === 0) {
      setBoundaryContextState(undefined);
      return;
    }

    setBoundaryContextState(next);
  }, []);

  const clearBoundaryContext = useCallback(() => {
    setBoundaryContextState(undefined);
  }, []);

  const value = useMemo<SuperpositionContextValue>(() => {
    const client = new SuperpositionClient(config);
    const hostContext = normalizedConfig.context;
    const mergedScope = mergeScopedContext(boundaryContext ?? {}, hostContext);
    const effectiveContext =
      Object.keys(mergedScope).length > 0 ? mergedScope : undefined;

    return {
      config,
      client,
      dimensions: dimensionsApi(client),
      defaultConfigs: defaultConfigsApi(client),
      overrides: overridesApi(client),
      experiments: experimentsApi(client),
      resolve: resolveApi(client),
      audit: auditApi(client),
      scope: {
        hostContext,
        boundaryContext,
        effectiveContext,
        lockedDimensions: getLockedDimensions(effectiveContext),
        hasBoundaryContext: Boolean(
          boundaryContext && Object.keys(boundaryContext).length > 0,
        ),
        setBoundaryContext,
        clearBoundaryContext,
      },
    };
  }, [
    boundaryContext,
    clearBoundaryContext,
    config,
    normalizedConfig,
    setBoundaryContext,
  ]);

  return (
    <SuperpositionContext.Provider value={value}>
      <ThemeProvider config={config}>{children}</ThemeProvider>
    </SuperpositionContext.Provider>
  );
}
