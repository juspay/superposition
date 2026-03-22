# Plan: Embeddable Admin UI for Superposition

## Current State Analysis

The Superposition admin UI is a **Leptos 0.6 full-stack application** with:
- **SSR + Hydration** architecture tightly coupled to Actix-web server
- **48 components** across config, dimensions, overrides, experiments, auth
- **reqwest-based API client** (`api.rs`, 1600+ lines) calling REST endpoints
- **Tailwind + DaisyUI** styling with theme support
- **Monaco editor** integration via custom wasm-bindgen bindings
- **Provider pattern** for alerts, CSR detection, editor state
- **Multi-tenant routing**: `/admin/:org_id/:workspace/*`

## Key Question: Leptos vs React SDK?

### Why NOT Leptos for embedding

1. **No web component support** — Leptos has no built-in custom element / shadow DOM story. You'd need to hand-roll `wasm-bindgen` registration for every component.

2. **SSR coupling** — The current architecture uses `SsrMode::Async`, server-side header context for auth cookies, and `leptos_actix` integration. None of this works when embedded in a third-party page.

3. **WASM bundle size** — The full frontend compiles to a single `frontend_bg.wasm` blob. There's no tree-shaking; embedding even a single page pulls in all 48 components, Monaco editor bindings, reqwest, serde, etc. Expect **2-5 MB gzipped**.

4. **Style isolation** — Tailwind generates global utility classes. Without shadow DOM (which Leptos doesn't support), these will clash with host page styles. DaisyUI's theme system would also conflict.

5. **Consumer DX** — Nobody outside Rust-WASM enthusiasts wants to `npm install` a Leptos widget. The ecosystem expects React/Vue/vanilla JS components.

6. **Hydration mismatch** — Leptos hydration assumes it rendered the initial HTML. When mounted into an arbitrary DOM node, this breaks entirely.

### Why React SDK is the right choice

1. **Ecosystem reach** — React components can be consumed by React, Next.js, Remix, and wrapped as web components for Vue/Angular/vanilla via `@lit/react` or similar.

2. **Tree-shakeable** — Consumers import only the pages they need: `import { ConfigManager } from '@superposition/admin-ui'`.

3. **Small bundles** — React + a REST client + Tailwind subset is **50-200 KB gzipped** vs multi-MB WASM.

4. **Style isolation options** — CSS Modules, Tailwind prefix, or shadow DOM wrapping are all straightforward.

5. **API already exists** — All 30+ REST endpoints in `api.rs` are plain HTTP. A TypeScript client is trivial to generate (or hand-write from the Smithy IDL that already exists in `superposition_sdk`).

6. **Context scoping is natural** — A React context provider accepting `{ dimensions: Record<string, string> }` maps directly to the existing `x-org-id` / `x-workspace` headers + dimension filters.

## Recommended Architecture

```
@anthropic/superposition-admin-ui (npm package)
├── packages/
│   ├── api-client/          # TypeScript REST client (generated from Smithy IDL or OpenAPI)
│   ├── admin-ui/            # React component library
│   │   ├── src/
│   │   │   ├── providers/
│   │   │   │   ├── SuperpositionProvider.tsx   # Top-level context (host, auth, scoped dimensions)
│   │   │   │   ├── AlertProvider.tsx
│   │   │   │   └── ThemeProvider.tsx
│   │   │   ├── components/                     # Shared UI (table, modal, form inputs)
│   │   │   ├── pages/                          # Feature pages
│   │   │   │   ├── ConfigManager.tsx           # Default configs CRUD
│   │   │   │   ├── DimensionManager.tsx        # Dimensions CRUD
│   │   │   │   ├── OverrideManager.tsx         # Context overrides CRUD
│   │   │   │   ├── ExperimentManager.tsx       # Experiments CRUD
│   │   │   │   ├── ResolveView.tsx             # Config resolution
│   │   │   │   └── AuditLog.tsx                # Audit log viewer
│   │   │   ├── hooks/                          # useConfig, useDimensions, useExperiments, etc.
│   │   │   └── index.ts                        # Public API
│   │   └── package.json
│   └── web-components/      # Optional: Custom element wrappers for non-React consumers
└── examples/
    ├── nextjs-embed/
    └── vanilla-html/
```

### Core API: `<SuperpositionProvider>`

```tsx
import { SuperpositionProvider, ConfigManager, OverrideManager } from '@superposition/admin-ui';

function MyDashboard() {
  return (
    <SuperpositionProvider
      host="https://superposition.example.com"
      orgId="my-org"
      workspace="production"
      // Optional: scope the UI to only show/manage configs matching this context
      context={{ region: "us-east-1", tenant: "acme" }}
      // Optional: auth token or cookie forwarding
      auth={{ token: "Bearer ..." }}
      // Optional: restrict which features are shown
      features={["config", "overrides", "experiments"]}
      // Optional: theme
      theme="light"
    >
      {/* Full admin UI with built-in routing */}
      <SuperpositionAdmin />

      {/* OR individual pages embedded where you want them */}
      <ConfigManager />
      <OverrideManager />
    </SuperpositionProvider>
  );
}
```

### Context Scoping Behavior

The `context` prop (a `Record<string, string>` of dimension → value) controls:

1. **Override filtering** — Only show overrides whose context is a subset of or matches the provided dimensions
2. **Override creation** — Pre-fill and optionally lock certain dimension values when creating new overrides
3. **Experiment filtering** — Only show experiments targeting the provided context
4. **Resolve view** — Pre-populate the resolve form with the scoped context
5. **Dimension visibility** — Optionally hide dimensions that are already "fixed" by the scoped context

This maps directly to the existing API's query parameters and the `context` field in override/experiment payloads.

## Implementation Plan

### Phase 1: API Client (TypeScript)
- Generate or hand-write a TypeScript client from the existing REST API
- Cover: configs, dimensions, overrides, experiments, experiment-groups, functions, types, webhooks, audit-log, resolve
- Auth: support Bearer token, cookie forwarding, and custom header injection
- The Smithy IDL in `superposition_sdk` can potentially generate this

### Phase 2: Core React Components
- Port the essential UI components from Leptos to React:
  - Table, Modal, Drawer, Form inputs, Dropdown, Pagination
  - Context form (dimension picker + value input)
  - JSON schema editor (replace Monaco with `@monaco-editor/react`)
- Build the provider hierarchy (SuperpositionProvider → AlertProvider → ThemeProvider)

### Phase 3: Feature Pages
- Port page-by-page, prioritized:
  1. **ConfigManager** (default configs) — most commonly needed
  2. **OverrideManager** (context overrides) — core value prop
  3. **DimensionManager** — needed for context understanding
  4. **ExperimentManager** — experimentation features
  5. **ResolveView** — config resolution testing
  6. **AuditLog** — compliance/debugging

### Phase 4: Context Scoping
- Implement the `context` prop filtering logic
- Add dimension locking (pre-filled, non-editable dimensions in forms)
- Filter API responses client-side or via query params

### Phase 5: Distribution
- npm package with ESM + CJS builds
- CSS as importable stylesheet with Tailwind prefix to avoid conflicts
- Optional web component wrappers (`<superposition-config-manager>`)
- CDN build for `<script>` tag usage

## Alternative: iframe Embedding (Quick Win)

If speed matters more than deep integration, a simpler first step:

```html
<iframe
  src="https://superposition.example.com/admin/my-org/production/default-config?embed=true&context=region:us-east-1"
  style="width:100%;height:600px;border:none;"
/>
```

This requires minimal changes to the existing Leptos app:
1. Add `?embed=true` query param support that hides the side nav and top bar
2. Add `?context=key:value,key:value` param for scoping
3. Use `postMessage` for cross-origin auth token passing

**Pros**: Ships in days, not weeks. **Cons**: No deep integration, limited customization, CORS/cookie issues.

## Recommendation

**Build the React SDK (Phases 1-5)** as the primary embeddable solution. It's the right long-term investment for adoption. The existing Leptos app continues as the standalone admin UI.

If there's urgency, **ship the iframe approach first** as a stopgap while the React SDK is built.

Keep the Leptos frontend as-is — it serves the standalone deployment well. The React SDK is a parallel artifact that shares only the REST API contract, not code.
