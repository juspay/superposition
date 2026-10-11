# /brag plan — Superposition

**What it is:** An open-source, context-aware configuration platform: typed defaults, overrides that resolve by context (city, tenant, region, device…), and rollouts through experiments instead of deploys.
**Who for:** Backend/frontend/platform teams who change behaviour in production (payments, pricing, LLM params) and are tired of redeploys and scattered env vars.
**What sets it apart:** Context-aware config first; feature flags are just one outcome. Predictable specificity, schema-validated values, experiments on top.
**Most impressive claim:** Change config safely without redeploying, with type safety and controlled rollouts.
**Visual hook:** The name + logo — Schrödinger's cat in a box. A config key that holds many values at once and "collapses" to one when you observe it with context.
**Real UI/flow to show:** The docs' own explainer data — `per_km_rate` default 10 → `city=bangalore` 12 → `bangalore + auto` 14 (weight 3); `webhook_url` "htps://api.payment .com/webhook" rejected; 100-user experiment grid. Same palette as the docs presentation (#0a0a0f bg, indigo #6366f1 / #818cf8 accent, green/red, Inter + JetBrains Mono).
**Tone:** default — punchy, clean, a little nerdy (quantum pun earns its place because it's the product's own name).
**Share caption:** Your config is in superposition. Superposition collapses it to the right value for every context — typed, validated, rolled out through experiments.

## Angle
"Schrödinger's config": one key, many true values, collapsed by context.

## Storyboard (landscape 1920×1080, 30fps, 22.0s, 120 BPM bed — cuts on bar lines)

| # | Time | Scene | On screen | Sound |
|---|---|---|---|---|
| 1 | 0.0–3.0 | Hook | `per_km_rate = ` with ghosted overlapping values 10 / 12 / 14 / 16 flickering. Line: "Your config is in superposition." | Filtered pad swell, shimmering arpeggio |
| 2 | 3.0–6.0 | Reveal | Values implode into the cat-in-box logo; wordmark **Superposition**; "Context-aware configuration. Open source." | Kick + bass drop in on 3.0 |
| 3 | 6.0–11.0 | Highlight 1: context collapses it | Context chip types `{ city: "bangalore", vehicle: "auto" }`; layers stack: default 10 → city 12 → city+vehicle 14 (weight 3); resolved value glows **14**. Caption "Add context. It collapses to the right value." | Soft key ticks under typing, chime on resolve |
| 4 | 11.0–15.0 | Highlight 2: typed + validated | `webhook_url` card, schema `"pattern": "^https://"`; value retyped to `htps://api.payment .com/webhook` → ✗ checks → **Rejected before it ships.** | Low muted thud on reject |
| 5 | 15.0–18.0 | Highlight 3: experiments | 100-dot user grid; 10% control / 10% variant light up. "Roll out through experiments. Not deploys." | Rising plucks per bucket |
| 6 | 18.0–22.0 | Outro | Logo + wordmark, `docker run -p 8080:8080 ghcr.io/juspay/superposition-demo:latest`, SDK row Rust · JavaScript · Python · Java · Haskell, `github.com/juspay/superposition` | Final chord ring-out |

Durations: 3 + 3 + 5 + 4 + 3 + 4 = 22s.
