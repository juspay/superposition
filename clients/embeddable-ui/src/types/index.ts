import type React from "react";
import type { JsonValue } from "./api";
export * from "./api";

export const SUPERPOSITION_FEATURES = ["config", "overrides", "dimensions"] as const;

export type SuperpositionFeature = (typeof SUPERPOSITION_FEATURES)[number];

export const SUPERPOSITION_FEATURE_LABELS: Record<SuperpositionFeature, string> = {
  config: "Configs",
  overrides: "Overrides",
  dimensions: "Dimensions",
};

export type RouteMode = "internal" | "external";
type InternalTransportMode = "same-origin" | "cross-origin" | "host-proxy";
export type AuthMode = "cookie" | "bearer" | "custom";
export type SuperpositionThemeMode = "light" | "dark" | "system";

export interface SuperpositionStyleConfig {
  padding?: string;
  margin?: string;
  width?: string;
  height?: string;
  textColor?: string;
  bgColor?: string;
  borderColor?: string;
  borderRadius?: string;
  fontSize?: string;
  fontWeight?: string;
  shadow?: string;
  textTransform?: string;
}

export interface SuperpositionThemeColors {
  bg?: string;
  panel?: string;
  text?: string;
  muted?: string;
  border?: string;
  primary?: string;
  success?: string;
  warning?: string;
  danger?: string;
}

export interface SuperpositionScaleConfig {
  xs?: string;
  sm?: string;
  md?: string;
  lg?: string;
}

export interface SuperpositionShadowConfig {
  sm?: string;
  md?: string;
}

export interface SuperpositionTypographyConfig {
  fontFamily?: string;
  fontSize?: string;
}

export interface SuperpositionButtonThemeConfig extends SuperpositionStyleConfig {
  primary?: SuperpositionStyleConfig;
  secondary?: SuperpositionStyleConfig;
  danger?: SuperpositionStyleConfig;
  disabledOpacity?: string;
}

export interface SuperpositionTableThemeConfig extends SuperpositionStyleConfig {
  header?: SuperpositionStyleConfig;
}

export interface SuperpositionFormThemeConfig extends SuperpositionStyleConfig {
  label?: SuperpositionStyleConfig;
  removeButton?: SuperpositionStyleConfig;
  helperTextColor?: string;
}

export interface SuperpositionDropdownThemeConfig extends SuperpositionStyleConfig {
  control?: SuperpositionStyleConfig;
  menu?: SuperpositionStyleConfig;
  option?: {
    hoverBgColor?: string;
    selectedBgColor?: string;
    selectedTextColor?: string;
  };
}

export interface SuperpositionIconThemeConfig {
  size?: string;
  color?: string;
  lock?: {
    size?: string;
    color?: string;
  };
}

export interface SuperpositionSearchThemeConfig extends SuperpositionStyleConfig {
  placeholderColor?: string;
  icon?: SuperpositionIconThemeConfig;
}

export interface SuperpositionToastThemeConfig extends SuperpositionStyleConfig {
  success?: SuperpositionStyleConfig;
  error?: SuperpositionStyleConfig;
  warning?: SuperpositionStyleConfig;
  info?: SuperpositionStyleConfig;
}

export interface SuperpositionBannerThemeConfig extends SuperpositionStyleConfig {
  warning?: SuperpositionStyleConfig;
  info?: SuperpositionStyleConfig;
  error?: SuperpositionStyleConfig;
  success?: SuperpositionStyleConfig;
}

export interface SuperpositionRequestContext {
  url: string;
  init: RequestInit;
}

export interface SuperpositionResponseContext {
  request: SuperpositionRequestContext;
  response: Response;
}

export interface SuperpositionAuthConfig {
  mode: AuthMode;
  token?: string;
  headers?: Record<string, string>;
}

interface InternalTransportConfig {
  mode: InternalTransportMode;
  baseUrl: string;
  apiBasePath?: string;
  credentials?: RequestCredentials;
  workspaceHeaderName?: "x-workspace" | "x-tenant";
}

export interface SuperpositionRoutingConfig {
  mode: RouteMode;
  initialFeature?: SuperpositionFeature;
  currentFeature?: SuperpositionFeature;
  onNavigate?: (feature: SuperpositionFeature) => void;
  getFeatureHref?: (feature: SuperpositionFeature) => string;
}

export interface SuperpositionThemeTokens {
  colors?: SuperpositionThemeColors;
  radius?: SuperpositionScaleConfig;
  spacing?: SuperpositionScaleConfig;
  shadow?: SuperpositionShadowConfig;
  typography?: SuperpositionTypographyConfig;
  button?: SuperpositionButtonThemeConfig;
  table?: SuperpositionTableThemeConfig;
  form?: SuperpositionFormThemeConfig;
  dropdown?: SuperpositionDropdownThemeConfig;
  icon?: SuperpositionIconThemeConfig;
  search?: SuperpositionSearchThemeConfig;
  toast?: SuperpositionToastThemeConfig;
  banner?: SuperpositionBannerThemeConfig;
  pageTitle?: SuperpositionStyleConfig;
  jsonValue?: SuperpositionStyleConfig;
  tooltip?: SuperpositionStyleConfig;
}

export interface SuperpositionThemeConfig extends SuperpositionThemeTokens {
  mode?: SuperpositionThemeMode;
}

export interface SuperpositionScopeConfig {
  context?: Record<string, JsonValue>;
  writeContext?: Record<string, JsonValue>;
  locked?: boolean;
}

export interface SuperpositionFilterConfig {
  defaultConfigPrefix?: string | string[];
  dimensions?: string[];
}

export interface ConfirmInput {
  title: string;
  description?: string;
  confirmLabel?: string;
  cancelLabel?: string;
  variant?: "default" | "destructive";
}

export interface SuperpositionUiAdapters {
  notify?: (input: {
    tone: "info" | "success" | "warning" | "error";
    title: string;
    description?: string;
  }) => void;
  confirm?: (input: ConfirmInput) => Promise<boolean>;
  renderModal?: (input: {
    open: boolean;
    onClose: () => void;
    title: string;
    children: React.ReactNode;
    footer?: React.ReactNode;
  }) => React.ReactNode;
  portalContainer?: Element | string | (() => Element | null);
  modalZIndex?: number;
  alertZIndex?: number;
  showBoundaryFilter?: boolean;
}

export interface SuperpositionFeatureCapabilities {
  create?: boolean;
  update?: boolean;
  delete?: boolean;
  ramp?: boolean;
  execute?: boolean;
  editContext?: boolean;
}

export type SuperpositionCapabilitiesConfig = Partial<
  Record<SuperpositionFeature, SuperpositionFeatureCapabilities>
>;

export interface SuperpositionLayoutConfig {
  adminContentMinHeight?: string;
  modalWidth?: string;
  modalMinWidth?: string;
  modalMaxWidth?: string;
  modalMaxHeight?: string;
  confirmWidth?: string;
  alertMinWidth?: string;
  tableMinWidth?: string;
  compactControlPadding?: string;
}

export interface SuperpositionNetworkHooks {
  interceptRequest?: (
    context: SuperpositionRequestContext,
  ) => Promise<SuperpositionRequestContext> | SuperpositionRequestContext;
  interceptResponse?: (
    context: SuperpositionResponseContext,
  ) => Promise<Response> | Response;
  onUnauthorized?: (response: Response) => void;
  onForbidden?: (response: Response) => void;
  onApiError?: (error: unknown) => void;
}

export interface SuperpositionEmbeddableConfig {
  apiBaseUrl: string;
  apiBasePath?: string;
  credentials?: RequestCredentials;
  workspaceHeaderName?: "x-workspace" | "x-tenant";
  orgId: string;
  workspace: string;
  auth?: SuperpositionAuthConfig;
  network?: SuperpositionNetworkHooks;
  scope?: SuperpositionScopeConfig;
  filters?: SuperpositionFilterConfig;
  capabilities?: SuperpositionCapabilitiesConfig;
  readOnly?: boolean;
  strict?: boolean;
  features?: SuperpositionFeature[];
  routing?: SuperpositionRoutingConfig;
  theme?: SuperpositionThemeConfig;
  layout?: SuperpositionLayoutConfig;
  ui?: SuperpositionUiAdapters;
  messages?: Record<string, string>;
}

export interface NormalizedSuperpositionConfig extends Omit<
  SuperpositionEmbeddableConfig,
  "apiBaseUrl" | "apiBasePath" | "credentials" | "workspaceHeaderName" | "scope"
> {
  transport: InternalTransportConfig;
  context?: Record<string, JsonValue>;
  lockScopedDimensions: boolean;
}
