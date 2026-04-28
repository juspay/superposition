# Superposition Embeddable UI

Embeddable React admin UI for Superposition configuration management.

## Package outputs

- Package: `superposition-embeddable-ui`
- React entry: `superposition-embeddable-ui`
- Browser helpers: `superposition-embeddable-ui/browser`
- Stylesheet: `superposition-embeddable-ui/styles.css`
- CDN bundle: `dist/superposition-embeddable-ui.global.js`

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
- `scope.locked` (optional): keeps the UI inside that bounded slice.
- `theme` (optional): color, typography, spacing, and radius overrides.

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
    "locked": true
  },
  "features": ["config", "overrides", "resolve"],
  "readOnly": false,
  "theme": {
    "mode": "light",
    "colorPrimary": "#0f766e",
    "colorDanger": "#dc2626",
    "colorBg": "#f8fafc",
    "colorPanel": "#ffffff",
    "colorText": "#111827",
    "colorMuted": "#6b7280",
    "colorBorder": "#d1d5db",
    "radiusMd": "8px",
    "radiusLg": "12px"
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
  locked: true,
},
```

If you leave `scope.context` out, the UI is unbounded.

## Theme

Theme values go directly into `config.theme`. The UI turns these values into
scoped CSS variables on the `.sp-ui` root, and all component styles read from
that variable contract.

```tsx
theme: {
  mode: "light",
  colorPrimary: "#0f766e",
  colorBg: "#ecfeff",
  colorPanel: "#ffffff",
},
```

Theme field effects:

| Token                                   | Affects                                                                              |
| --------------------------------------- | ------------------------------------------------------------------------------------ |
| `colorBg`                               | outer admin surface background                                                       |
| `colorPanel`                            | cards, tables, modal panels, inputs, secondary buttons, primary button text          |
| `colorText`                             | primary text, secondary button text, neutral feedback text                           |
| `colorMuted`                            | helper text, table headers, empty states, neutral badges                             |
| `colorBorder`                           | table, modal, card, input, secondary button, and derived feedback borders            |
| `colorPrimary`                          | primary buttons, active tabs, focus rings, selection, info badges, active pagination |
| `colorSuccess`                          | success alerts and in-progress/status badges                                         |
| `colorWarning`                          | read-only/scope notices, paused/status badges, locked condition badges               |
| `colorDanger`                           | destructive buttons, errors, delete/discard badges                                   |
| `radiusSm`                              | compact controls, code blocks, inline notices                                        |
| `radiusMd`                              | inputs, primary/secondary buttons, filter panels                                     |
| `radiusLg`                              | cards, tables, modal panels, admin shell panels                                      |
| `spaceXs`/`spaceSm`/`spaceMd`/`spaceLg` | shell-level spacing                                                                  |
| `shadowSm`/`shadowMd`                   | shell, toast, and modal elevation                                                    |
| `fontFamily`/`fontSize`                 | inherited typography for the embedded surface                                        |

## React usage

```tsx
import "superposition-embeddable-ui/styles.css";
import {
  AlertProvider,
  SuperpositionAdmin,
  SuperpositionProvider,
} from "superposition-embeddable-ui";

export function EmbeddedAdminPage() {
  return (
    <SuperpositionProvider
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
          colorPrimary: "#4f46e5",
          colorBg: "#f8fafc",
          colorPanel: "#ffffff",
        },
      }}
    >
      <AlertProvider>
        <SuperpositionAdmin />
      </AlertProvider>
    </SuperpositionProvider>
  );
}
```

Use `SuperpositionAdmin` for the full shell, or mount individual surfaces like `DefaultConfigManager`, `OverridesManager`, `DimensionsManager`, `ResolveView`, and `AuditLog`.

Granular embeds can still use the same provider config. For example, this renders only default configs under `checkout.` and shows values resolved for the fixed host scope:

```tsx
<SuperpositionProvider
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
    <DefaultConfigManager />
  </AlertProvider>
</SuperpositionProvider>
```

For scoped overrides, the fixed `scope.context` is applied automatically. The create form only lets users choose default config keys and values; it does not let them edit the fixed context.

## Custom elements

```html
<script type="module">
  import { defineCustomElements } from "superposition-embeddable-ui/browser";

  defineCustomElements();
</script>

<superposition-admin
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
      "colorPrimary": "#0f766e",
      "colorBg": "#ecfeff",
      "colorPanel": "#ffffff"
    }
  }'
></superposition-admin>
```

Supported tags include:

- `superposition-admin`
- `superposition-config-manager`
- `superposition-override-manager`
- `superposition-dimension-manager`
- `superposition-experiment-manager`
- `superposition-resolve-view`
- `superposition-audit-log`

## Script tag embedding

```html
<link rel="stylesheet" href="/assets/superposition-embeddable-ui/styles.css" />
<div id="sp-admin"></div>
<script src="/assets/superposition-embeddable-ui/superposition-embeddable-ui.global.js"></script>
<script>
  const mount = window.SuperpositionEmbeddableUI.mountSuperpositionFeature;

  mount("#sp-admin", "admin", {
    config: {
      apiBaseUrl: "/api",
      orgId: "localorg",
      workspace: "production",
      scope: {
        context: { region: "us-east-1" },
        locked: true,
      },
      theme: {
        mode: "light",
        colorPrimary: "#4f46e5",
        colorBg: "#f8fafc",
        colorPanel: "#ffffff",
      },
      features: ["config", "overrides", "resolve"],
    },
  });
</script>
```

The global bundle exposes:

- `window.SuperpositionEmbeddableUI.mountSuperpositionFeature`
- `window.SuperpositionEmbeddableUI.defineCustomElements`
- `window.SuperpositionEmbeddableUI.customElementTagNames`

## Demo hosts

- React host demo: `npm run demo` then open `demo/index.html`
- Custom element host demo: `npm run demo` then open `demo/custom-elements.html`

## Notes

- Import the stylesheet once per host app when you use the React or module-based browser entry.
- Use the browser entry when the host is not React-aware but can load ESM modules.
- Use the global bundle when the host only supports plain `<script>` tags.
- `scope.context` and `scope.locked` let hosts pre-scope the UI to a fixed operational slice.
