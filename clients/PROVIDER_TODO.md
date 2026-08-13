# OpenFeature Provider — Remaining Work (TODO)

**Open** work for the cross-language provider parity effort across **Rust**
(`crates/superposition_provider`), **Python** (`clients/python/provider`),
**Java** (`clients/java/openfeature-provider`), and **JavaScript**
(`clients/javascript/open-feature-provider`).

Branch: `provider/updated` — **nothing committed yet.**

> Completed & verified items have been moved to **`PROVIDER_DONE.md`**. This file
> tracks only what is still open.

---

## Legend
- `[ ]` not started · `[~]` partial / discussed · **DISCUSS** = needs a decision first

---

## 0b. Lower-severity / SHOULD deviations (open)

- [ ] Rust: file watcher not torn down when last subscriber leaves (SHOULD, §7).
  - **My recommendation: low priority / likely defer.** Now that RS-LEAK is fixed, the `notify` watcher is released by RAII when the data source drops (the common case). The residual gap is only "`watch()` was called, all consumers stopped, but the provider lives on" — a minor lingering-handle case. If we do it: drop the shared `WatcherInner` when the broadcast `receiver_count()` hits 0. Not worth prioritizing over §6/commit.
- [ ] **File-watch event-filter divergence — all four (DISCUSS, §7).** Each language decides "is this event about my file?" differently, with edge-case gaps:
  - **Rust / JS** filter on **file name equality only** (basename compare). Simplest; blind to symlink indirection and to dropped-event overflow.
  - **Python** filters on **realpath equality** (`os.path.realpath`) — strongest identity match, follows symlinks; costs a stat per event.
  - **Java** matches **filename OR a non-`Path` context** — i.e. on `OVERFLOW` (OS dropped events) it re-reads defensively. The only impl that survives event-queue overflow; the others silently miss the change.
  - Two gaps to decide/close for uniformity: (a) **overflow/dropped-event robustness** — should Rust/JS/Python also re-read defensively when the watcher signals overflow (or a rename with no usable name), matching Java? (b) **symlink resolution** — should realpath-style matching (Python) be the standard everywhere, or name-only (Rust/JS)? Pick one canonical behavior in `PROVIDER_SPEC.md` §7 and align the other three.
  - **Suggested canonical (my recommendation):**
    - (a) **Adopt Java's defensive re-read as canonical.** Silently missing a config change is worse than an occasional spurious reload, so on an overflow/rescan signal — or any event with no usable filename — re-read. JS already does this (the null-`filename` case fires anyway); Java does (`OVERFLOW`). Add it to Rust (`notify` surfaces rescan/error events) and Python (watchdog) where the backend exposes it; where it doesn't, leave a comment noting the gap. Cheap and strictly safer.
    - (b) **Adopt realpath as canonical.** Resolve the target via realpath at watch setup and match within the *real* parent dir, so a symlinked config path is watched correctly. JS already realpaths at setup; Python realpaths per event; only Rust matches by bare filename — have Rust realpath the target once at setup. One extra stat, no hot-path cost.
    - Then encode both in `PROVIDER_SPEC.md` §7.
- [ ] Java: RefreshStrategy defaults (30000/60000/300000) not encoded; no default-OnDemand when none configured (§9).
  - **My recommendation: keep explicit (don't add a default-when-null), but add convenience factories.** Java requiring an explicit strategy is defensible — no hidden behavior. The ergonomic gap is only that a caller must spell out `RefreshStrategy.Polling(30000, 60000)` with magic numbers. Fix that with `RefreshStrategy.polling()` / `.onDemand()` factory methods that carry the canonical defaults (mirroring Rust `Default`/Python `default_*_strategy()`), rather than defaulting a *null* strategy. Low priority; ergonomics, not correctness.
- [ ] Java: out-of-int32-range integral values rejected as TYPE_MISMATCH (platform-forced by 32-bit `Integer`; documented, borderline). **Leave** — platform limit, not fixable.

## 1. MED review findings — open

| # | Sev | Finding | Languages affected | Spec ID |
|---|-----|---------|--------------------|---------|
| 9 | MED | Sync `resolve` path skips `ensureFreshData` (no OnDemand refresh on sync calls) — see §7 recommendation (document as a platform limitation) | Python | `PY-SYNC` |

*(MED findings 1–8 and 10 are resolved/accepted — see `PROVIDER_DONE.md`.)*

## 1a. Remove deprecated legacy providers

The old pre-architecture providers are **wrong and deprecated** — they are not to
be reconciled, only deleted. They are explicitly out of scope in the spec (§1).

- [ ] Delete Rust legacy `SuperpositionProvider` (`provider.rs`) + its `CacConfig`/`ExperimentationConfig` client path (`client.rs`) once nothing depends on it.
- [ ] Delete Python legacy `SuperpositionProvider` (`provider.py`) + `configuration_client.py` / `exp_config.py` / `cac_config.py` as appropriate.
- [ ] Delete JS legacy `SuperpositionProvider` (`superposition-provider.ts`) + `configuration-client.ts` / `experimentation-client.ts` + the lenient `utils.ts` coercion helpers.
- [ ] Delete Java legacy `SuperpositionOpenFeatureProvider` + `RefreshJob` / `SuperpositionConfig` if superseded.
- [ ] Repoint any CI / provider-sdk-tests still on a legacy provider to `LocalResolutionProvider` (see §6), then drop the legacy exports from each package `index`.

## 2. LOW review findings — open

- [ ] Java: applicable-variant strings vs structured — cosmetic. (Already consistent across langs — `List<String>` everywhere — likely no action.)
- [ ] Misleading docstrings / comments referencing old shapes — sweep. (Fold into the §8 standalone-docs sweep.)

## 3. PrefixQueryInterceptor follow-ups (deferred — "do later")

- [ ] Move `PrefixQueryInterceptor` into the **SDK** module, parallel to `client/auth/`, opt-in like auth.
- [ ] Make it schema-driven (detect list-typed `@httpQuery` members generically, not a hardcoded 4-input `instanceof` chain).
- [ ] Relocate `SdkListQuerySerializationTest` into the SDK module.
- [ ] Add an **API-provider-specific** regression test: assert `prefix`/`exclude_prefix` reach the wire for `ApplicableVariants` + `GetResolvedConfigWithIdentifier`, exactly once.
- [ ] Probe whether a smithy-java upgrade serializes list `@httpQuery` members → retire the interceptor.

## 5. Kotlin conversion (analysis only — not a quick win)

- [ ] Convert `traits/AllFeatureProvider.java` + `FeatureExperimentMeta.java` to Kotlin. Requires `-Xjvm-default=all` in the Kotlin convention plugin (else Java implementers lose the `default` methods via `DefaultImpls`) + `@Throws`. Do as part of a fuller Kotlin migration, not in isolation. (Pairs with §11 — Kotlin's nullable `T?` is exactly the `@Nullable T` target shape.)

## 6. Verification / housekeeping (before or alongside commit)

- [ ] Run full test suites — **none run end-to-end yet** (only the Rust `leak_tests` have actually executed):
  - [ ] Rust: `cargo test -p superposition_provider`
  - [ ] Python: `pytest clients/python/provider`
  - [ ] JS: `npm test` (jest) in `open-feature-provider`
  - [ ] Java: full `gradle test` (blocked by the stale UniFFI native-lib checksum — run `make uniffi-bindings` first to regenerate from the current core)
  - [ ] Integration: `make test-<lang>-provider` (needs Docker for the DB/redis + a live server)
- [ ] Rebuild JS provider `dist/` (`node build-deps.js && npm run build`) — current bundle predates the changes.
- [ ] Commit `provider/updated` once suites are green.

## 7. Spec — open decisions

- [ ] **Open decisions (implementation-defined in the spec, pending a call here), then encode the chosen canonical behavior in the spec body:**
  - **WATCH-FILTER — my recommendation:** see the sub-bullets under the §0b `WATCH-FILTER` item (adopt Java's defensive re-read for overflow; adopt realpath matching for symlinks).
  - **PY-SYNC — my recommendation: document as a platform limitation; do NOT force a fragile sync-over-async bridge.** Under OnDemand + the *synchronous* OpenFeature resolve path, Python cannot `await` the async refresh, so it serves cached data and logs a hint — and because OnDemand has no background loop, that cache stays the init-time snapshot until an *async* resolve refreshes it. Resolution: (1) keep the current behavior (warn + serve cache); (2) document clearly that **OnDemand freshness requires the `*_async` resolution methods in Python** (the sync path is best-effort/cached); (3) the spec §13 already states this as an idiomatic-per-language limitation — leave it. Only revisit if a *robust* blocking bridge (a dedicated event-loop thread the sync call briefly joins) can be built **and validated against a live server** — a bare `asyncio.run()` is not acceptable (breaks under an already-running loop and reuses an async SDK client across loops). Net: lowest-risk, and it doesn't make the sync path lie about freshness.

## 8. Make each provider's docs standalone (remove cross-language references)

Each provider's comments and docstrings should read as if that language's
implementation is the only one — self-sufficient, with no need to consult another
language to understand any aspect. `PROVIDER_SPEC.md` is the single place
cross-language equivalence is documented.

- [ ] Sweep all four providers (Rust / Python / Java / JS) and remove cross-language
  reference phrasing from comments/docstrings: "matching Rust", "mirrors Python",
  "same shape as Rust", "like JS", "the Rust/Java equivalent", "anchored on Rust",
  "(matching the Python/Java file sources)", etc. Many were added during the parity
  work — **including a lot in this session** (e.g. the `REMOTE-SHUT` guard comments,
  `refreshTimeoutMs` docs, the `watch()` sync-contract notes, the RefreshStrategy
  `object Manual` note, single-flight comments).
- [ ] **Preserve the rationale, drop the reference.** The valuable "why" stays, but
  restated on its own terms. E.g. `"…empty (unbounded) for Watch/Manual — mirroring
  Rust's Option<Duration>"` → `"…empty when the strategy runs unbounded (Watch,
  Manual); only Polling and OnDemand carry a timeout."` The behavior/reasoning is
  explained standalone, not by pointing at another language.
- [ ] Ensure every non-obvious behavior is **fully documented in place** — after the
  sweep, no comment should require reading another language's source to be understood.
- [ ] Guideline going forward: put cross-language canonical behavior in
  `PROVIDER_SPEC.md`; keep per-language code comments about *that language's* behavior
  and reasoning only.
- [ ] (Supersedes the "misleading docstrings" sweep noted in §2.)

## 9. Mandatory function typing (JS + Python) + build-time enforcement

Every function/method in the JS and Python providers should carry a full
signature — explicit parameter types **and** an explicit return type. Add the
annotations wherever missing, then make it a build-time gate so it can't regress.

### 9a. Add the missing signatures
- [ ] **JS/TS** (`clients/javascript/open-feature-provider/`): add explicit **return
  types** on every function/method/arrow that lacks one (`tsconfig` `strict:true`
  already forbids implicit-`any` params, so params are largely covered — the gap is
  return types and any stray `any`).
- [ ] **Python** (`clients/python/provider/`): add parameter + return annotations on
  every `def`/`async def` that lacks them (e.g. the file-watch handlers just typed,
  and any others surfaced by the linter below).

### 9b. Enforce at build time (so new code must be typed)
- [ ] **TypeScript** — add ESLint + `@typescript-eslint` with
  `explicit-function-return-type` and `explicit-module-boundary-types` enabled;
  add an `npm run lint` script and run it in the provider build / CI (fail on error).
- [ ] **Python** — enable a type gate in `clients/python/provider/pyproject.toml`
  (none exists today). Either **mypy** (`disallow_untyped_defs = true`,
  `disallow_incomplete_defs = true`, ideally `check_untyped_defs = true`) or **ruff**
  with the `ANN` rule set (`ANN001` missing arg annotation, `ANN201` missing return).
  Wire it into the `test-py-provider` make target / CI so an untyped `def` fails the build.
- [ ] Decide scope: enforce on the provider packages first; consider extending the same
  gate to the `provider-sdk-tests` harnesses.

## 10. JS provider: correct dual-format packaging for publish (ESM + CJS, JS + TS)

The published `superposition-provider` package must be cleanly consumable by both
**JavaScript and TypeScript** users, in both **ESM (`import`) and CommonJS
(`require`)** projects. The current `package.json` *attempts* this (`main`=cjs,
`module`=esm, `types`, an `exports` map) but has known correctness pitfalls to audit
and fix:

- [ ] **`exports` condition ordering** — `types` is currently listed **last**; under
  `moduleResolution: node16/nodenext` conditions resolve **in order**, so `types` must
  come **first** in each entry, before `import`/`require`. Reorder to
  `{ "types": …, "import": …, "require": … }`.
- [ ] **CJS-file-in-an-ESM-package footgun** — the package is `"type": "module"`, yet
  `main` points at `./dist/index.js` holding **CJS** output. Node treats a `.js` file
  in a `"type":"module"` package as **ESM**, so `require()` consumers get a broken/
  misinterpreted entry. Emit the CommonJS build as **`.cjs`** (e.g. `dist/index.cjs`)
  and point `main`/`exports.require` at it (rollup `format:'cjs'` → `.cjs`).
- [ ] **Per-format type declarations** — under node16 resolution, ship `index.d.ts`
  for the ESM condition and `index.d.cts` for the CJS condition (or verify a single
  `.d.ts` resolves correctly for both); today only one `.d.ts` is emitted.
- [ ] **Verify, don't assume** — add `publint` and `@arethetypeswrong/cli` (`attw`) to
  the build/CI to catch exports/types mismatches automatically.
- [ ] **Fresh-install smoke test** — from a packed tarball (`npm pack`), confirm all of:
  (a) ESM `import { LocalResolutionProvider } from "superposition-provider"`,
  (b) CJS `const { LocalResolutionProvider } = require("superposition-provider")`,
  (c) a TS project resolves the types under both `node16` and `bundler` resolution.
- [ ] **Native FFI in the tarball** — the package bundles `superposition-bindings`
  (`koffi` native FFI) + `superposition-sdk` via `bundledDependencies`; verify the
  published artifact actually loads the native lib on a clean install (per-platform),
  not just in this repo's workspace layout.

## 11. Design discussion (Java): `Optional<T>` as method **parameter** types

**DISCUSS — decide, then apply.** IntelliJ flags `'Optional' used as type for parameter`
across the Java provider. It's a widely-held Java anti-pattern (Brian Goetz, the
JDK's own API author, intended `Optional` for **return** types only): as a
parameter it forces every caller to wrap (`Optional.of(x)` / `Optional.empty()`),
adds a wrapper allocation, and — the irony — the `Optional` reference **can itself
be null**, so it doesn't even guarantee what it advertises.

**Scope (pervasive — it's on the canonical contract):**
- `SuperpositionDataSource` (interface, §5 canonical surface): `context`,
  `prefixFilter`, `excludePrefixFilter`, `ifModifiedSince` are all `Optional<...>`
  across the fetch methods (~14 signatures).
- `FileDataSource` / `HttpDataSource` mirror those.
- The local/remote provider resolve methods take `Optional<List<String>>`
  prefix/exclude filters.

**The cross-language angle (this is really a "native per language" call, like the
error-rendering decision):** every other client already expresses "optional param"
in *its* idiom — Rust `Option<T>`, Python `Optional[T]`, JS `T | undefined` / `?`.
Java's *idiomatic* "optional parameter" is **not** `Optional<T>` — it's a nullable
`@Nullable T`. So `Optional<T>` params make Java the odd one out **against its own
language's conventions**, not just against a linter.

**Options:**
- **A — keep `Optional<T>` params.** Self-documenting "this is optional"; zero work.
  But IntelliJ-warned, non-idiomatic, verbose call sites, extra allocation.
- **B — switch to `@Nullable T` params (recommended).** Java-idiomatic, silences the
  inspection, matches how the other three express optionality in their own idioms.
  Cost: reintroduces `null` at these boundaries (needs `!= null` checks internally,
  a few of which already exist), and adding a `@Nullable`/JSR-305/JSpecify annotation
  dependency (or a local annotation) for intent.
- **C — overloads (with/without each optional arg).** Impractical: 4 optional params
  ⇒ combinatorial method explosion.
- **D — a `FetchOptions`/params record.** Cleaner signatures, but a bigger refactor
  and diverges from the other clients' flat parameter lists.

**Recommendation:** **B** — it's the same principle we used for error rendering
(§3.1, Option C): each language renders/expresses in *its own* idiom rather than a
forced-uniform shape. Then update `PROVIDER_SPEC.md` §5 / §1.4 to state explicitly
that **parameter optionality is expressed idiomatically per language** (Rust
`Option`, Python `Optional`, JS `?`, Java `@Nullable`) — the canonical contract
fixes the method **names and semantics**, not the wrapper used to signal "optional".

**Note:** this is a **contract-surface** change (the `SuperpositionDataSource`
signatures are part of §1.4/§5), so it needs the spec clarification above, and it
should be done together with the §5 Kotlin-conversion consideration if that happens
(Kotlin expresses this as plain nullable `T?`, which is exactly the target shape).
