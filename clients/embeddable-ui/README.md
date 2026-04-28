# Superposition Embeddable UI

Embeddable React admin UI for Superposition configuration management.

## Package outputs

- Package: `superposition-embeddable-ui`
- React entry: `superposition-embeddable-ui`
- Browser helpers: `superposition-embeddable-ui/browser`
- Stylesheet: `superposition-embeddable-ui/styles.css`
- Split self-contained local assets:
  - `dist/vendor/react.production.min.js`
  - `dist/vendor/react-dom.production.min.js`
  - `dist/superposition-browser-core.global.external.js`
- Externalized CDN bundle: `dist/superposition-embeddable-ui.global.external.js`
- Per-feature externalized globals:
  - `dist/superposition-admin.global.external.js`
  - `dist/superposition-config-manager.global.external.js`
  - `dist/superposition-override-manager.global.external.js`
  - `dist/superposition-dimension-manager.global.external.js`

## Packaging this for another app

If your friend wants to embed this today, there are three practical distribution paths:

1. Publish the package to npm and let them install it.
2. Run `npm pack` in this package and share the generated tarball for a private install.
3. Ship the `dist/` artifacts and let them load the global bundle or browser entry directly.

Typical release flow:

```bash
cd clients/embeddable-ui
npm run build
npm pack
```

That produces a tarball your friend can install with:

```bash
npm install /path/to/superposition-embeddable-ui-0.1.0.tgz
```

If you plan to publish publicly or internally, rename the package to your final scope first.

## What a host app supplies

The host app only needs to provide a `config` object.

- `apiBaseUrl`: where the embeddable UI should send REST requests.
- `orgId` and `workspace`: which Superposition workspace to manage.
- `scope.context` (optional): a bounded filter for scoped embedding.
- `scope.writeContext` (optional): a narrower boundary for create/edit actions.
- `scope.locked` (optional): keeps the UI inside that bounded slice.
- `strict` (optional): prevents extra boundary context and uses exact context matching for override lists.
- `features` (optional): which screens are allowed to render. `[]` renders no feature UI.
- `capabilities` (optional): per-feature action switches such as create, delete, ramp, and execute.
- `filters.defaultConfigPrefix` (optional): restricts default config keys and override values.
- `theme` (optional): color, typography, spacing, and radius overrides.
- `layout` (optional): host-controlled shell, modal, and alert sizing.
- `ui` (optional): host-owned notifications, confirmation, layering, and boundary-filter controls.
- `messages` (optional): host overrides for visible SDK copy.

The embeddable UI currently uses its own fetch-based REST client. It does not call a host-installed Superposition SDK directly. If your host app already talks to Superposition through its own backend or SDK, expose or proxy those REST endpoints from the host and point `apiBaseUrl` at that proxy.

```json
{
  "apiBaseUrl": "/api",
  "orgId": "localorg",
  "workspace": "production",
  "scope": {
    "context": {
      "region": "us-east-1",
      "tenant": "acme"
    },
    "locked": true,
    "strict": true
  },
  "features": ["config", "dimensions", "overrides"],
  "readOnly": false,
  "capabilities": {
    "config": { "create": false, "delete": false },
    "dimensions": { "create": false, "delete": false },
    "overrides": { "create": true, "update": true }
  },
  "filters": {
    "defaultConfigPrefix": "checkout.",
    "dimensions": ["region", "tenant"]
  },
  "theme": {
    "mode": "light",
    "colors": {
      "primary": "#0f766e",
      "danger": "#dc2626",
      "bg": "#f8fafc",
      "panel": "#ffffff",
      "text": "#111827",
      "muted": "#6b7280",
      "border": "#d1d5db"
    },
    "radius": { "md": "8px", "lg": "12px" },
    "spacing": { "sm": "10px", "md": "14px" },
    "typography": { "fontSize": "14px" },
    "table": {
      "header": { "bgColor": "#f3f4f6", "textColor": "#111827" }
    },
    "button": {
      "padding": "10px 18px",
      "borderRadius": "8px",
      "primary": { "bgColor": "#7c3aed", "textColor": "#ffffff" },
      "secondary": { "bgColor": "#ffffff", "textColor": "#111827" },
      "danger": { "bgColor": "#fee2e2", "textColor": "#991b1b" }
    },
    "jsonValue": { "borderColor": "#d1d5db" },
    "form": {
      "label": { "fontWeight": "700" },
      "removeButton": {
        "bgColor": "#9333ea",
        "textColor": "#ffffff",
        "width": "48px",
        "height": "44px"
      }
    },
    "icon": {
      "size": "16px",
      "color": "#4b5563",
      "lock": { "color": "#6b7280" }
    },
    "search": {
      "width": "420px",
      "borderRadius": "12px",
      "placeholderColor": "#9ca3af",
      "icon": { "color": "#6b7280" }
    },
    "pageTitle": {
      "fontSize": "28px",
      "fontWeight": "800",
      "textColor": "#374151"
    },
    "banner": {
      "bgColor": "#fff7ed",
      "textColor": "#92400e",
      "borderColor": "#fed7aa"
    },
    "toast": {
      "bgColor": "#ffffff",
      "textColor": "#111827",
      "success": { "borderColor": "#22c55e" }
    },
    "dropdown": {
      "width": "320px",
      "option": { "selectedBgColor": "#f3f4f6" }
    },
    "tooltip": {
      "bgColor": "#111827",
      "textColor": "#f8fafc",
      "borderRadius": "6px"
    }
  },
  "layout": {
    "adminContentMinHeight": "520px",
    "modalWidth": "min(720px, calc(100vw - 32px))",
    "confirmWidth": "min(440px, calc(100vw - 32px))",
    "tableMinWidth": "680px",
    "compactControlPadding": "4px 8px",
    "alertMinWidth": "280px"
  },
  "ui": {
    "showBoundaryFilter": false,
    "modalZIndex": 1200,
    "alertZIndex": 1200
  },
  "messages": {
    "config.create": "Add config",
    "common.readOnly": "View only"
  }
}
```

## API base URL

Hosts do not need to choose a connection mode. The SDK infers that internally from `apiBaseUrl`.

- Use `/api` when the host backend proxies Superposition REST calls.
- Use a full URL such as `https://superposition.example.com` when the browser should call Superposition directly.

In practice, the request target is `apiBaseUrl` plus optional `apiBasePath`. Advanced hosts can still set `credentials`, `workspaceHeaderName`, and request or response hooks.

## Optional bounded filter

The bounded filter belongs in `config.scope.context` because it affects list views, create flows, and scoped locking across the whole embedded surface.

```tsx
scope: {
  context: { region: "us-east-1", tenant: "acme" },
  writeContext: { tenant: "acme" },
  locked: true,
},
strict: true,
```

If you leave `scope.context` out, the overrides UI is view-only: it can list
overrides, but it will not show create or edit actions. If `scope.writeContext`
is supplied, override create/edit actions are limited to combinations inside
that write boundary; otherwise they use `scope.context`.

Set top-level `strict: true` when the host scope should be the whole boundary.
In strict mode, users cannot add extra boundary filter context, and the admin
boundary filter is hidden by default. Override lists send the scoped context to
the List Contexts API as `dimension[...]` query params; strict mode sends
`dimension_match_strategy=exact`, while the default sends `subset`.

## Capabilities

`readOnly` is still the fastest way to disable all mutating actions. Use
`capabilities` when the host needs more precise control.

For overrides, create and update actions also require a write boundary
(`scope.writeContext` or `scope.context`). This keeps embedded hosts from
accidentally exposing global override edits.

```tsx
capabilities: {
  config: { create: false, delete: false },
  dimensions: { create: false, delete: false },
  overrides: { create: true, update: true },
},
```

## Host UI hooks

The SDK ships default alerts, modals, and confirmations so demos work quickly.
Production host apps can take over those pieces through `config.ui`.

```tsx
ui: {
  notify: ({ tone, title }) => hostToast[tone](title),
  confirm: ({ title, description }) => hostDialog.confirm({ title, description }),
  renderModal: ({ title, children, footer, onClose }) =>
    hostModal.render({ title, children, footer, onClose }),
  portalContainer: "#host-overlays",
  showBoundaryFilter: false,
},
```

If `notify` is provided, the SDK does not render its own toast stack. If
`confirm` is provided, destructive actions use the host dialog instead of the
SDK fallback dialog. `renderModal` lets React hosts replace SDK modal chrome for
create, ramp, and fallback confirmation flows. `portalContainer`, `modalZIndex`,
and `alertZIndex` let the host control overlay placement and layering.

## Theme

Theme values go directly into `config.theme`. The preferred shape groups styles
by component area, with a shared `SuperpositionStyleConfig` shape for common
properties such as `padding`, `width`, `height`, `textColor`, `bgColor`,
`borderColor`, `borderRadius`, `fontSize`, `fontWeight`, and `shadow`.
The UI turns these values into scoped CSS variables on the `.sp-ui` root, and all
component styles read from that variable contract.

```tsx
theme: {
  mode: "light",
  colors: {
    primary: "#0f766e",
    bg: "#ecfeff",
    panel: "#ffffff",
  },
  icon: {
    color: "#4b5563",
    size: "16px",
  },
  button: {
    padding: "10px 18px",
    borderRadius: "8px",
    primary: {
      bgColor: "#7c3aed",
      textColor: "#ffffff",
    },
    secondary: {
      bgColor: "#ffffff",
      textColor: "#111827",
      borderColor: "#d1d5db",
    },
  },
  table: {
    header: {
      bgColor: "#f3f4f6",
      textColor: "#111827",
    },
  },
  form: {
    label: {
      fontWeight: "700",
    },
    removeButton: {
      bgColor: "#9333ea",
      textColor: "#ffffff",
      width: "48px",
      height: "44px",
    },
  },
  search: {
    width: "420px",
    borderRadius: "12px",
    placeholderColor: "#9ca3af",
    icon: {
      color: "#6b7280",
    },
  },
  pageTitle: {
    fontSize: "28px",
    fontWeight: "800",
  },
  banner: {
    bgColor: "#fff7ed",
    textColor: "#92400e",
    borderColor: "#fed7aa",
  },
  toast: {
    bgColor: "#ffffff",
    success: {
      borderColor: "#22c55e",
    },
  },
},
```

Theme field effects:

| Token                                                                   | Affects                                                                 |
| ----------------------------------------------------------------------- | ----------------------------------------------------------------------- |
| `colors.bg`/`colors.panel`/`colors.text`/`colors.muted`/`colors.border` | outer surfaces, panels, default text, muted copy, and borders           |
| `colors.primary`/`colors.success`/`colors.warning`/`colors.danger`      | accent, feedback, and semantic state colors                             |
| `radius.sm`/`radius.md`/`radius.lg`                                     | compact, control, and card/modal radius scale                           |
| `spacing.xs`/`spacing.sm`/`spacing.md`/`spacing.lg`                     | shell-level spacing scale                                               |
| `shadow.sm`/`shadow.md`                                                 | shell, toast, modal, and tooltip elevation                              |
| `button`                                                                | shared button sizing, typography, border radius, and disabled opacity   |
| `button.primary`/`button.secondary`/`button.danger`                     | per-variant button colors and elevation                                 |
| `table.header`                                                          | table header background, label color, type, spacing, and text transform |
| `form.label`/`form.helperTextColor`/`form.removeButton`                 | form labels, helper copy, and override form delete icon button          |
| `icon.color`/`icon.size`/`icon.lock`                                    | default neutral/action icons and locked-condition icon styling          |
| `search`/`search.icon`                                                  | search box shape, color, placeholder, and search icon styling           |
| `pageTitle`                                                             | top-level page heading typography and color                             |
| `banner`                                                                | read-only and fixed-scope banner styling                                |
| `toast`/`toast.success`/`toast.error`/`toast.warning`/`toast.info`      | toast styling and per-tone colors                                       |
| `dropdown.control`/`dropdown.menu`/`dropdown.option`                    | structured override form dropdown styling                               |
| `jsonValue`                                                             | inline and expanded JSON value presentation                             |
| `tooltip`                                                               | icon button tooltip colors, shape, elevation, and type size             |

## React usage

```tsx
import "superposition-embeddable-ui/styles.css";
import {
  AlertProvider,
  SuperpositionAdmin,
  SuperpositionUIProvider,
} from "superposition-embeddable-ui";

export function EmbeddedAdminPage() {
  return (
    <SuperpositionUIProvider
      config={{
        apiBaseUrl: "/api",
        orgId: "localorg",
        workspace: "production",
        scope: {
          context: { region: "us-east-1" },
          locked: true,
        },
        theme: {
          mode: "light",
          colors: {
            primary: "#4f46e5",
            bg: "#f8fafc",
            panel: "#ffffff",
          },
          icon: {
            color: "#4b5563",
          },
          button: {
            primary: {
              bgColor: "#7c3aed",
              textColor: "#ffffff",
            },
            secondary: {
              bgColor: "#ffffff",
              textColor: "#111827",
            },
          },
        },
      }}
    >
      <AlertProvider>
        <SuperpositionAdmin />
      </AlertProvider>
    </SuperpositionUIProvider>
  );
}
```

Use `SuperpositionAdmin` for the full shell. Routing config is consumed by this
shell because it owns the tabs and feature navigation. Individual surfaces such
as `ConfigManager`, `OverrideManager`, and `DimensionManager` do not navigate on
their own; they just render the one feature the host mounted.

Granular embeds can still use the same provider config. For example, this renders only default configs under `checkout.` and shows values resolved for the fixed host scope:

```tsx
<SuperpositionUIProvider
  config={{
    apiBaseUrl: "/api",
    orgId: "localorg",
    workspace: "production",
    scope: {
      context: { tenant: "acme", region: "us-east-1" },
      locked: true,
    },
    filters: {
      defaultConfigPrefix: "checkout.",
    },
  }}
>
  <AlertProvider>
    <ConfigManager />
  </AlertProvider>
</SuperpositionUIProvider>
```

For scoped overrides, the fixed write boundary is applied automatically. The
create form only lets users choose default config keys and values; it does not
let them edit the fixed context.

## Offerings

`superposition-embeddable-ui` is split into two offerings:

- Package API: use the explicit subpaths like `superposition-embeddable-ui/admin` or `superposition-embeddable-ui/config-manager` inside apps that already use a bundler.
- Browser API: use `superposition-embeddable-ui/browser`, the externalized all-in-one global, or the shared-core plus per-feature globals for no-bundler embedding and custom elements.

## Browser Path Chooser

| Path                                                                 | Use when                                                                | Host loads                                                          | Tradeoff                                                          |
| -------------------------------------------------------------------- | ----------------------------------------------------------------------- | ------------------------------------------------------------------- | ----------------------------------------------------------------- |
| `superposition-embeddable-ui/browser`                                | the host can load ESM modules                                           | one module entry plus lazy-loaded feature chunks                    | best fit for modern non-React hosts with a module-capable browser |
| `superposition-embeddable-ui.global.external.js`                     | the host wants one browser global API                                   | React and ReactDOM separately                                       | simplest all-in-one browser API without bundling React twice      |
| `superposition-browser-core.global.external.js` + per-feature global | the host wants the smallest script-tag payload                          | React, ReactDOM, shared core, and only the feature wrapper it needs | best script-tag path when the host only needs one or two features |
| `dist/vendor/*` + shared core + per-feature global                   | the host wants self-hosted browser assets with no public CDN dependency | local vendored React assets, shared core, and feature wrapper       | preferred self-contained script-tag path                          |

## Custom elements

```html
<script type="module">
  import {
    defineCustomElements,
    registerSuperpositionHostAdapters,
  } from "superposition-embeddable-ui/browser";

  registerSuperpositionHostAdapters("host-app", {
    auth: { mode: "custom", headers: { "x-host-auth": "..." } },
    ui: {
      notify: ({ tone, title }) => hostToast[tone](title),
      confirm: ({ title }) => hostDialog.confirm(title),
    },
  });

  defineCustomElements();
</script>

<superposition-admin
  adapter-id="host-app"
  config='{
    "apiBaseUrl": "/api",
    "orgId": "localorg",
    "workspace": "production",
    "scope": {
      "context": { "region": "us-east-1" },
      "locked": true
    },
    "theme": {
      "mode": "light",
      "colors": {
        "primary": "#0f766e",
        "bg": "#ecfeff",
        "panel": "#ffffff"
      }
    }
  }'
></superposition-admin>
```

Custom element `config` is JSON, so function callbacks cannot live directly in
the attribute. Use `registerSuperpositionHostAdapters()` plus `adapter-id` when
the host needs callbacks for auth, network hooks, notifications, confirms, or
modal rendering.

Supported tags include:

- `superposition-admin`
- `superposition-config-manager`
- `superposition-override-manager`
- `superposition-dimension-manager`

## Lighter externalized global

If the host page already loads React and ReactDOM, use the externalized global instead of the self-contained bundle.

```html
<script crossorigin src="https://unpkg.com/react@18/umd/react.production.min.js"></script>
<script
  crossorigin
  src="https://unpkg.com/react-dom@18/umd/react-dom.production.min.js"
></script>
<link rel="stylesheet" href="/assets/superposition-embeddable-ui/styles.css" />
<script src="/assets/superposition-embeddable-ui/superposition-embeddable-ui.global.external.js"></script>
<script>
  window.SuperpositionEmbeddableUI.mountSuperpositionFeature("#sp-admin", "admin", {
    config: {
      apiBaseUrl: "/api",
      orgId: "localorg",
      workspace: "production",
    },
  });
</script>
```

This externalized bundle keeps the same `window.SuperpositionEmbeddableUI` API as the self-contained bundle. The only difference is that React and ReactDOM must already be present on the page.

## Split self-contained local assets

If you want a self-contained distribution without relying on public CDNs, serve the vendored React files from `dist/vendor` and then load the shared browser core.

```html
<script src="/assets/superposition-embeddable-ui/vendor/react.production.min.js"></script>
<script src="/assets/superposition-embeddable-ui/vendor/react-dom.production.min.js"></script>
<script src="/assets/superposition-embeddable-ui/superposition-browser-core.global.external.js"></script>
<script src="/assets/superposition-embeddable-ui/superposition-admin.global.external.js"></script>
<script>
  window.SuperpositionAdminUI.mount("#sp-admin", {
    config: {
      apiBaseUrl: "/api",
      orgId: "localorg",
      workspace: "production",
    },
  });
</script>
```

This is the preferred self-contained browser path now. It avoids the large single-file monolith and lets hosts load only the feature wrapper they need.

For a runnable copy-paste example, see `demo/shared-core-feature-globals.html` or run `npm run demo:shared-core`.

## Per-feature global bundles

If the host only needs one feature, load the matching per-feature global instead of the all-in-one bundle.

```html
<script crossorigin src="https://unpkg.com/react@18/umd/react.production.min.js"></script>
<script
  crossorigin
  src="https://unpkg.com/react-dom@18/umd/react-dom.production.min.js"
></script>
<link rel="stylesheet" href="/assets/superposition-embeddable-ui/styles.css" />
<script src="/assets/superposition-embeddable-ui/superposition-browser-core.global.external.js"></script>
<div id="sp-configs"></div>
<script src="/assets/superposition-embeddable-ui/superposition-config-manager.global.external.js"></script>
<script>
  window.SuperpositionConfigManagerUI.mount("#sp-configs", {
    config: {
      apiBaseUrl: "/api",
      orgId: "localorg",
      workspace: "production",
    },
  });
</script>
```

Per-feature globals expose:

- `window.SuperpositionAdminUI`
- `window.SuperpositionConfigManagerUI`
- `window.SuperpositionOverrideManagerUI`
- `window.SuperpositionDimensionManagerUI`

Each per-feature global exports:

- `mount(container, { config, props? })`
- `defineCustomElement(prefix?)`
- `registerSuperpositionHostAdapters(id, adapters)`
- `unregisterSuperpositionHostAdapters(id)`
- `customElementTagName`

Per-feature globals now require `superposition-browser-core.global.external.js` first, so shared browser runtime and provider code is loaded once instead of repeated in every feature file.

## Demo hosts

- React host demo: `npm run demo` then open `demo/index.html`
- Custom element host demo: `npm run demo` then open `demo/custom-elements.html`
- Shared core + feature global demo: `npm run demo:shared-core`

## Notes

- Import the stylesheet once per host app when you use the React or module-based browser entry.
- Use the browser entry when the host is not React-aware but can load ESM modules.
- Use the global bundle when the host only supports plain `<script>` tags.
- `scope.context` and `scope.locked` let hosts pre-scope the UI to a fixed operational slice.
