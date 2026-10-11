# /brag plan — Superposition (v2)

**What it is:** Open-source, context-aware configuration: typed defaults, overrides resolved by context, experiments instead of deploys.
**Focus for v2 (requested):** the one-line local-first setup from juspay/superposition#1162 (`LocalResolutionProvider::auto()` + `super.toml`), growing into the full platform, and agents using config as a safe mitigation lever.
**Angle:** "Your config is in superposition." Start with one line and a file, grow into a platform, hand the lever to your agents.
**Tone:** default; punchy, tactile, a little playful.

## Visual identity
Taken from the cat-in-a-box logo instead of generic SaaS indigo:
- Paper `#F3EADC` / `#FBF6EE`, ink `#1C1512`, cardboard `#DE8A45` / `#B9652E`
- Lab glass: blue `#3460E8`, acid green `#2FAE5F`, alert red `#E4442F`
- Bricolage Grotesque (display) + JetBrains Mono (code)
- Hard ink outlines with offset "cardboard" shadows, dot-grid paper, subtle grain

## Motion system
Masked line reveals, spring physics for cards/stickers, one shared editor object that carries across scenes (types → switches tab → shrinks into a dashboard tile), skewed cardboard/ink panel wipes at section changes, chart line drawn in sync with the agent log.

## Storyboard (1920×1080, 30fps, 25s, 120 BPM)
| # | Time | Scene |
|---|---|---|
| 1 | 0–2.5 | Hook: "Your config is in superposition." Logo box with 10/12/14/16 orbiting as flickering states |
| 2 | 2.5–7.5 | main.rs: `let provider = LocalResolutionProvider::auto()?;` typed; `cargo run` → "✓ loaded ./super.toml · watching for edits"; "One line to start." + no server / no token / no database |
| 3 | 7.5–11.5 | Tab → super.toml (PR example); `max_retries = 5` edited to 8 → app card flips 5→8, "reloaded" |
| 4 | 11.5–16 | Editor shrinks into a tile labelled "offline fallback"; 4 env vars → "same line, now server-backed"; tiles: Contexts, Experiments, Validation, Audit log, OpenFeature providers |
| 5 | 16–21.5 | Dark: error-rate chart spikes after `checkout_v2 → 20%`; on-call agent traces it and calls `discard_experiment("checkout_v2")`; recovers. "Fixed with config, not code." |
| 6 | 21.5–25 | Logo, wordmark, `LocalResolutionProvider::auto()?`, github link |

Notes: the agent scene illustrates a pattern (agent + Superposition API); it does not name or imply any specific company. `auto()` is from an open PR (juspay/superposition#1162) and is not yet released.
