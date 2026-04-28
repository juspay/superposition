# Embeddable Admin UI: React Decision and Embedding Contract

## Summary

We should keep the existing Leptos admin UI as the standalone Superposition admin application, and build the embeddable admin UI as a React package.

This is not a rejection of Leptos for the standalone app. The current Leptos app is good for a first-party, fully owned Superposition surface. The problem is embedding that surface inside an arbitrary host application while still respecting the host's routing, styling, layout, and interaction model.

## Why React for embeddable UI, not Leptos

### 1. Leptos is the wrong artifact for third-party embedding

The current Leptos frontend is a standalone app artifact. It assumes Superposition owns the page, the route tree, the styling environment, and the application shell.

An embeddable UI has different requirements:
- it mounts inside someone else's DOM
- it must coexist with someone else's router
- it must inherit or adapt to someone else's design system
- it must not leak styles or force a new app shell
- it must expose configuration hooks that a host frontend can control

That is a package problem, not just a component problem.

### 2. WASM splitting is a real constraint

With the current Leptos approach, the frontend is effectively shipped as a WASM application bundle. That creates a poor fit for embeddable distribution:
- feature-level tree-shaking is weak compared to standard React library builds
- the host cannot easily import just one screen or one workflow
- the WASM blob tends to pull in more runtime than needed for a narrow embedded use case
- loading strategy becomes app-oriented, not component-oriented

In practice, this means we do not get clean feature-level splitting for embedding. The host wants to import only what it uses, for example only overrides or only config resolution. A React package can expose that cleanly with normal ESM imports.

### 3. Leptos would still need to remain separate

Even if we tried to force embedding through Leptos, we would still need the standalone admin UI to keep running as its own application.

So the realistic end state is not “Leptos everywhere”. It is:
- Leptos continues as the standalone Superposition admin application
- React becomes the embeddable admin UI package

That split is healthy because the two surfaces solve different problems:
- standalone admin: Superposition owns the whole experience
- embeddable admin: the host owns the outer experience, Superposition provides feature UI

## Product direction

The immediate goal is not to invent a new UX. The goal is to preserve the same functional design and operator familiarity from the current Leptos admin UI, while changing the packaging and ownership model.

That means:
- keep the same core feature flows
- keep the same domain language
- keep the same information hierarchy where it works
- keep visual parity close enough that existing operators recognize the product
- refactor the implementation into headless logic plus skinnable presentation

## Relationship to the current Leptos component design

Yes, the intention is to respect the current Leptos feature design, but with an important distinction.

We should preserve:
- the same feature boundaries
- the same operator workflows
- the same page-level information architecture
- the same domain concepts and form structure
- the same shared UI patterns where they make sense

We should not preserve, as-is:
- the standalone app shell assumptions
- the Leptos route ownership model
- the hard coupling between page, layout, and navigation
- the assumption that Superposition owns modals, toasts, and global styling

So the React interfaces in this doc are not meant to be a pixel-for-pixel rewrite of only the top-level pages. They are meant to preserve the current Leptos component design at the feature and workflow level, while splitting it into host-safe interfaces.

## Concrete mapping from today's Leptos UI

The current Leptos app already has a useful component structure, and the React package should follow that structure closely.

### Existing Leptos shell structure

Today the Leptos app is organized roughly like this:
- `Layout` and `CommonLayout` own the application shell
- `SideNav` and `OrgSideNav` own navigation
- `AlertProvider` and `Toast` own alerts
- route definitions in `app.rs` own feature navigation

For embedding, we should keep this design conceptually, but split it differently:
- `SuperpositionAdmin` becomes the optional default shell
- individual feature exports become the embeddable entry points
- routing becomes configurable instead of fixed
- alerts, dialogs, and telemetry become adapters instead of hardcoded UI ownership

### Existing Leptos feature structure

Today the Leptos frontend already has feature-specific components such as:
- `DefaultConfigList`, `DefaultConfigForm`
- `ContextOverride`, `OverrideForm`
- `Dimensions`, `DimensionForm`
- `ExperimentList`, `ExperimentPage`, `ExperimentForm`, `ExperimentRampForm`, `ExperimentConcludeForm`
- `AuditLog`
- shared primitives like `Table`, `Pagination`, `Modal`, `Drawer`, `Dropdown`, `Button`, `Toast`

This is the part we should preserve most strongly.

The React package should keep the same feature decomposition, for example:
- config management remains a first-class feature
- overrides remain a first-class feature
- dimensions remain a first-class feature
- experiments remain a first-class feature with the same major actions
- audit and resolve remain separate features

## What the current React interfaces preserve already

The interfaces in this doc already preserve several important parts of the Leptos design:
- feature-level exports such as config, overrides, dimensions, experiments, resolve, and audit
- a provider-based shared context model similar to today's provider pattern
- an optional top-level shell through `SuperpositionAdmin`
- a shared configuration object for org, workspace, auth, and scoped context

That means they already respect the current Leptos design at the level of:
- feature boundaries
- shared runtime context
- page composition entry points

## What is still missing to truly match the Leptos component design

What the current interfaces do not yet express strongly enough is the lower-level component and workflow mapping.

To fully respect today's Leptos design, the React package should also define stable interfaces for:
- shell slots, such as header, navigation, breadcrumbs, and page actions
- form-level controllers for config, override, dimension, and experiment flows
- action-level components for create, edit, delete, ramp, conclude, and resolve
- primitive UI replacements for table, pagination, modal, drawer, dropdown, and alerts

In other words, the current doc gets the outer embedding contract right, but it still needs one more layer that mirrors the Leptos internal component breakdown more explicitly.

## Recommended design rule

The safest rule is:

- preserve Leptos feature design and workflow design
- preserve shared component concepts where they are reusable
- do not preserve app-shell ownership, route ownership, or global-style ownership

That is the right balance between familiarity and true embeddability.

## What “headless and skinnable” means here

### Headless

The embeddable package must expose reusable logic and state management without forcing a layout or shell.

Examples:
- fetch configs, overrides, experiments, audit entries
- manage form state and mutations
- enforce scoped context rules
- expose loading, empty, error, and success states
- expose actions such as create, update, ramp, conclude, resolve

### Skinnable

The embeddable package must allow the host application to control presentation without rewriting product logic.

Examples:
- host-provided colors, typography, spacing, radii, shadows
- host-provided navigation model
- host-provided dialogs, toasts, and portal containers
- host-controlled shell, tabs, breadcrumbs, and page titles
- ability to use default Superposition skin or fully custom rendering

## Non-negotiable embedding rules

The embeddable UI must respect the host application.

That means the package must not:
- assume ownership of the full page
- assume ownership of the top-level router
- inject broad global CSS that can collide with host styles
- require a fixed theme or app shell
- assume modal, toast, or navigation behavior
- assume every host uses React Router, Tailwind, or the same auth mechanism

## Required configurability

The embeddable contract needs to be strong enough for real product integration. At minimum, the following areas must be configurable.

### 1. Backend and auth
- backend base URL or API base path
- org id
- workspace
- transport mode: same-origin, cross-origin, or host-proxy
- credentials mode
- auth mode: cookie, bearer, or custom
- bearer token or custom auth headers
- optional request interception for special host auth setups
- optional response interception for token refresh or centralized error handling
- unauthorized and forbidden handlers for host-owned auth flows
- optional compatibility with alternate workspace header conventions such as `x-tenant`

### 2. Context scoping
- scoped dimensions passed from the host
- locking selected scoped dimensions in forms
- hiding or disabling context edits when the host wants strict scope
- feature-specific filtering based on host scope

### 3. Routing
- internal routing mode for demo or standalone-like usage
- external routing mode where the host owns navigation
- route mapping for each feature
- callbacks for navigation events
- deep-link compatibility for host URLs

### 4. Shell and layout
- render with Superposition default shell
- render feature screens without shell
- mount feature views individually
- host-provided page chrome, breadcrumbs, headers, tabs, and side navigation

### 5. Styling and theming
- default Superposition theme for quick adoption
- host token overrides for color, typography, spacing, radius, border, shadow
- class name prefixing or namespacing strategy
- optional CSS variable contract
- predictable portal and layering configuration

### 6. UX infrastructure
- host-provided toast adapter
- host-provided confirm dialog adapter
- host-provided modal or drawer adapter when needed
- host-provided telemetry hooks
- host-provided i18n strings or message overrides

### 7. Feature gating
- enable only selected features
- control per-feature capabilities such as create, edit, delete, ramp, conclude
- support read-only mode where required

## Frontend interfaces we should write

The exact names can still be adjusted, but the interface shape should be stable and explicit.

```ts
export type SuperpositionFeature =
  | "config"
  | "overrides"
  | "dimensions"
  | "experiments"
  | "resolve"
  | "audit";

export type RouteMode = "internal" | "external";
export type RenderMode = "default-shell" | "headless";
export type TransportMode = "same-origin" | "cross-origin" | "host-proxy";
export type AuthMode = "cookie" | "bearer" | "custom";

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

export interface SuperpositionTransportConfig {
  mode: TransportMode;
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
  colorBg?: string;
  colorPanel?: string;
  colorText?: string;
  colorMuted?: string;
  colorBorder?: string;
  colorPrimary?: string;
  colorSuccess?: string;
  colorWarning?: string;
  colorDanger?: string;
  fontFamily?: string;
  fontSize?: string;
  radiusSm?: string;
  radiusMd?: string;
  radiusLg?: string;
  spaceXs?: string;
  spaceSm?: string;
  spaceMd?: string;
  spaceLg?: string;
  shadowSm?: string;
  shadowMd?: string;
}

export interface SuperpositionUiAdapters {
  notify?: (input: {
    tone: "info" | "success" | "warning" | "error";
    title: string;
    description?: string;
  }) => void;
  confirm?: (input: {
    title: string;
    description?: string;
    confirmLabel?: string;
    cancelLabel?: string;
  }) => Promise<boolean>;
  trackEvent?: (name: string, payload?: Record<string, unknown>) => void;
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
  transport: SuperpositionTransportConfig;
  orgId: string;
  workspace: string;
  auth?: SuperpositionAuthConfig;
  network?: SuperpositionNetworkHooks;

  context?: Record<string, string | number | boolean | null>;
  lockScopedDimensions?: boolean;
  readOnly?: boolean;

  features?: SuperpositionFeature[];
  renderMode?: RenderMode;
  routing?: SuperpositionRoutingConfig;

  classNamePrefix?: string;
  theme?: {
    mode?: "light" | "dark" | "system";
    tokens?: SuperpositionThemeTokens;
  };

  ui?: SuperpositionUiAdapters;
}
```

## Backend integration contract

The config above is the minimum shape that is safe for real embedding.

### Why the original smaller config is not enough

The smaller set of:
- backend host
- org id
- workspace
- credentials mode
- bearer token or custom auth headers
- request interception

is enough only for controlled integrations where:
- the host and backend are same-origin, or
- the host proxies requests to the backend, and
- the auth mechanism is already known in advance

It is not strong enough as the default contract for a general embeddable package because it does not model:
- cross-origin transport constraints
- cookie-based browser auth flows
- alternate workspace header conventions
- response interception and token refresh
- host-controlled handling for `401` and `403` responses

### Practical rule

If we want the embeddable package to work reliably across internal dashboards, SaaS host apps, and proxied integrations, then backend interactivity should be defined through four explicit pieces:

1. `transport`
- where requests go
- whether they are same-origin, cross-origin, or proxied
- whether credentials are sent

2. `auth`
- whether authentication is cookie-based, bearer-based, or fully custom
- what headers or token material are attached

3. `network`
- how the host intercepts requests and responses
- how unauthorized, forbidden, and generic API errors are handled

4. `orgId` and `workspace`
- the backend scoping values required for most workspace-specific endpoints

## Core React surface we should expose

```ts
export interface SuperpositionProviderProps {
  config: SuperpositionEmbeddableConfig;
  children: React.ReactNode;
}

export interface SuperpositionAdminProps {
  defaultFeature?: SuperpositionFeature;
}

export function SuperpositionProvider(props: SuperpositionProviderProps): JSX.Element;
export function SuperpositionAdmin(props: SuperpositionAdminProps): JSX.Element;

export function ConfigManager(): JSX.Element;
export function OverrideManager(): JSX.Element;
export function DimensionManager(): JSX.Element;
export function ExperimentManager(): JSX.Element;
export function ResolveView(): JSX.Element;
export function AuditLog(): JSX.Element;
```

## Headless-first split we should move toward

To stay truly embeddable, each feature should eventually have two layers.

### 1. Logic layer
Hooks and controllers that understand Superposition behavior.

Examples:
- `useConfigManager()`
- `useOverrideManager()`
- `useExperimentManager()`
- `useResolveView()`
- `useDefaultConfigForm()`
- `useOverrideForm()`
- `useDimensionForm()`
- `useExperimentForm()`

These should expose:
- data
- mutations
- loading state
- empty state
- validation state
- scoped context helpers
- permissions and feature capability checks

### 2. Presentation layer
Composable React components that consume the logic layer.

Examples:
- `ConfigManagerView`
- `OverrideManagerView`
- `ExperimentManagerView`
- `ResolveViewPanel`
- `DefaultConfigFormView`
- `OverrideFormView`
- `DimensionFormView`
- `ExperimentFormView`

This lets us ship:
- default Superposition-styled views
- host-skinned versions using the same logic
- shell-free embedding inside an existing host page

## Recommended product split

### Keep
- the current Leptos admin UI as the standalone admin application

### Build
- a React embeddable admin package
- feature-level exports
- headless hooks plus skinnable default views
- a small optional default shell for quick adoption and demos

## Final decision

We should not try to embed the current Leptos app artifact directly into host applications.

We should:
- keep Leptos for the standalone admin product
- use React for the embeddable package
- preserve the existing Leptos feature design and workflows
- rebuild the UI as headless and skinnable React surfaces
- lock down the embedding contract up front so routing, styling, auth, and host-owned UX remain configurable and safe

That gives us the best of both worlds: the current standalone admin stays intact, and the embeddable UI becomes a proper frontend package that respects host applications instead of fighting them.

## Example host integration

```tsx
function AcmeDashboard() {
  const route = useAcmeRouter();

  return (
    <AcmeShell>
      <SuperpositionProvider
        config={{
          transport: {
            mode: "host-proxy",
            baseUrl: "",
            apiBasePath: "/api/sp-proxy",
          },
          orgId: "acme-corp",
          workspace: "production",
          auth: { mode: "cookie" },

          context: { environment: "production", region: "eu-west" },
          lockScopedDimensions: true,

          routing: {
            mode: "external",
            currentFeature: route.spFeature,
            onNavigate: (feature) => route.push(`/settings/flags/${feature}`),
            getFeatureHref: (feature) => `/settings/flags/${feature}`,
          },

          renderMode: "headless",
          features: ["config", "overrides", "experiments"],

          classNamePrefix: "sp",
          theme: {
            mode: "dark",
            tokens: {
              colorPrimary: "#E94560",
              fontFamily: "'Roboto', sans-serif",
              radiusMd: "8px",
            },
          },

          ui: {
            notify: ({ tone, title }) => acmeToast[tone](title),
            confirm: ({ title }) => acmeDialog.confirm(title),
            trackEvent: (name, data) => analytics.track(name, data),
          },
        }}
      >
        <SuperpositionAdmin />
      </SuperpositionProvider>
    </AcmeShell>
  );
}
```

This is the target integration shape: the host owns the outer shell, routing, theme decisions, and UX infrastructure, while the Superposition package owns only the feature UI and backend interaction contract.
