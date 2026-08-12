# OpenFeature Provider — Remaining Work (TODO)

Status of the cross-language provider parity effort across **Rust**
(`crates/superposition_provider`), **Python** (`clients/python/provider`),
**Java** (`clients/java/openfeature-provider`), and **JavaScript**
(`clients/javascript/open-feature-provider`).

Branch: `provider/updated` — **nothing committed yet.**

---

## Legend
- `[ ]` not started · `[~]` partial / discussed · `[x]` done & verified
- **Sev**: severity from the cross-language review (HIGH already cleared)

---

## 0. Conformance validation run (against PROVIDER_SPEC.md)

A full conformance validation of all four languages against the spec surfaced
**new** deviations beyond the original review. Cross-language ones are added to
spec §20 (IDs referenced below); single-language bugs are listed here.

> **Re-evaluated against current code (2026-08-09):** all §0a MUST violations
> below are **still open** — none were touched by the single-flight / executor
> work. Line references updated to the current source (several had drifted).
> **Both sides verified:** for every cross-cutting finding the "correct"
> reference language was also checked and genuinely is correct, so the scoping is
> accurate — `CTX-RESET`/`EXT-CASE`/`WATCH-GUARD` correct in Rust+Java;
> `REMOTE-SHUT` correct in JS; `WATCH-TIMEOUT` correct in Rust+Python+JS;
> `EXP-INIT-STR` canonical in Python+JS+Java.

### 0a. MUST violations (fix first) — all still open

| Lang | Finding | Spec ref |
|---|---|---|
| ~~**Python**~~ ✅ | ~~Remote `_resolve_remote` re-raises the raw SDK error with a bare `raise`~~ **DONE** — now wraps as `NETWORK_ERROR`, passes `SuperpositionError` through | §14 |
| ~~**Python**~~ ✅ | ~~Watch strategy never checks the primary can watch~~ **DONE (option 2)** — normalized `watch()` to a **sync** `def watch() -> Optional[AsyncGenerator]` (matches Rust/Java/JS — capability check is sync, async lives on the returned `_watch_stream()`); `_start_refresh_strategy` calls it and raises `CONFIG_ERROR` on `None` | §9.3 → `WATCH-GUARD` |
| ~~**Python**~~ ✅ | ~~`shutdown()` does not reset `global_context`~~ **DONE** — resets to `EvaluationContext()` (JS resets to `{}`); closes `CTX-RESET` across all four | §12.3 → `CTX-RESET` |
| ~~**Python**~~ ✅ | ~~File extension check case-sensitive~~ **DONE (option 2)** — extracts extension via `os.path.splitext` (JS: `path.extname`), lowercases, compares; matches Rust/Java | §7 → `EXT-CASE` |
| ~~**Python**~~ ✅ | ~~Remote provider: no post-shutdown guard~~ **DONE** — `_require_client()` guards resolve + variants → `PROVIDER_ERROR "…is shut down"` | §14 → `REMOTE-SHUT` (== MED #6) |
| ~~**JS**~~ ✅ | ~~Watch strategy silently returns when primary can't watch~~ **DONE** — `startWatch` now throws `CONFIG_ERROR` on a null stream (contract was already `AsyncGenerator \| null`; guard only) | §9.3 → `WATCH-GUARD` |
| ~~**JS**~~ ✅ | ~~`shutdown()` does not reset `globalContext`~~ **DONE** — resets to `{}` (see Python row) | §12.3 → `CTX-RESET` |
| ~~**JS**~~ ✅ | ~~File extension check case-sensitive~~ **DONE (option 2)** — `path.extname(...).toLowerCase()` compare (see Python row) | §7 → `EXT-CASE` |
| ~~**Rust**~~ ✅ | ~~Remote provider has no shutdown/guard~~ **DONE (reframed)** — added `ensure_ready()` **pre-init** guard → `ProviderError "…is not ready"`. Post-shutdown is **N/A for Rust**: `open_feature` has no shutdown hook and teardown is `Arc`-drop (the `Client` releases itself), so there's no live-but-shut-down state (same platform bucket as `RS-EVENTS`). `close_provider` was removed as an uncalled method. | §14 → `REMOTE-SHUT` |
| ~~**Rust**~~ ✅ | ~~Experiment-init error string differs from canonical~~ **DONE** — now `"…no experiment-capable fallback configured: {e}"`, matches Python/JS/Java | §12.2 → `EXP-INIT-STR` |
| ~~**Rust**~~ ✅ | ~~`FileDataSource::new` returns untyped `String`~~ **DONE** — now `Result<Self>` returning `DataSourceError`, message aligned; 4 `.unwrap()` callers unaffected (build incl. examples+tests green) | §7 / §3.1 |
| ~~**Java**~~ ✅ | ~~API provider has no post-shutdown guard~~ **DONE** — `requireReady()` before the try in resolve + variants (status-based, option 1) → `providerError "…is not ready"`; client is `final`/not-closeable so no teardown | §14 → `REMOTE-SHUT` |
| ~~**Java**~~ ✅ | ~~Watch/Manual refresh is timeout-bounded~~ **DONE (model fix, anchored on Rust)** — removed `timeoutMilliseconds` from the sealed interface; `Watch` = `debounceMs` only, `Manual` = config-less `object`; `refreshTimeoutMs()` returns a timeout only for Polling/OnDemand (Watch/Manual unbounded). Same field-model fix applied to JS `options.ts`. Legacy provider patched (not deleted) to keep compiling. | §10.5 → `WATCH-TIMEOUT` |

### 0b. Lower-severity / SHOULD deviations

- [ ] Empty prefix/exclude lists not omitted at SDK-command layer — **Python, JS** (`EMPTY-FILTER`, §16). Rust/Java omit.
- [x] ~~No single-in-flight-refresh guard (SHOULD) — all four~~ **DONE (§17):** concurrent `refresh()` callers now coalesce onto one in-flight refresh in all four languages, so an OnDemand TTL-expiry burst causes one fetch, not N. Java: shared `Future` + owner-cancels-on-timeout, outcome recorded on the worker. Python: shared `asyncio` task (`_run_one_refresh`) awaited via `shield`. JS: shared self-clearing `Promise` (`runOneRefresh`). Rust: `futures_util::future::Shared` over a `Weak`-capturing boxed future (required `SuperpositionError: Clone`). Compiles/typechecks in all four; full test runs still blocked by the stale-native-lib env issue.
- [ ] Rust: remote resolve forwards empty `""` identifier instead of omitting (§14). Also Python resolve identifier not omitting empty (`remote_provider.py:289`).
- [ ] Rust: file watcher not torn down when last subscriber leaves (SHOULD, §7).
- [ ] **File-watch event-filter divergence — all four (DISCUSS, §7).** Each language decides "is this event about my file?" differently, with edge-case gaps:
  - **Rust / JS** filter on **file name equality only** (basename compare). Simplest; blind to symlink indirection and to dropped-event overflow.
  - **Python** filters on **realpath equality** (`os.path.realpath`) — strongest identity match, follows symlinks; costs a stat per event.
  - **Java** matches **filename OR a non-`Path` context** — i.e. on `OVERFLOW` (OS dropped events) it re-reads defensively. The only impl that survives event-queue overflow; the others silently miss the change.
  - Two gaps to decide/close for uniformity: (a) **overflow/dropped-event robustness** — should Rust/JS/Python also re-read defensively when the watcher signals overflow (or a rename with no usable name), matching Java? (b) **symlink resolution** — should realpath-style matching (Python) be the standard everywhere, or name-only (Rust/JS)? Pick one canonical behavior in `PROVIDER_SPEC.md` §7 and align the other three.
- [ ] Python/JS: core `evalConfig` failure surfaces raw error, not coded `CONFIG_ERROR` (observable OF code still GENERAL; §13.5).
- [ ] Python: `fetched_at` has no "fallback to now" when server omits `last_modified` (§2.2).
- [ ] Python: init `global_context = context` with no null→empty default (§12.2).
- [ ] JS: GENERAL error message inserts `typeName` — `"Error evaluating {typeName} flag '{key}'…"` vs spec `"Error evaluating flag '{key}'…"` (§3.2).
- [x] ~~JS list-query interceptor~~ — **RESOLVED (verified 2026-07):** the JS smithy-typescript SDK *does* serialize list `@httpQuery` members (`Aws_restJson1.ts:1124-1135` etc. — `prefix`/`exclude_prefix` arrays → `.q(query)`). Rust (aws-smithy-rust, `get_config.rs:139-149`) and Python (smithy-python, `serialize.py:1051/1375/157`) likewise serialize lists correctly. **Only smithy-java is broken** (already fixed via `PrefixQueryInterceptor` + `SdkListQuerySerializationTest`). No interceptor needed for Rust/Python/JS. Re-verify on SDK upgrades.
- [ ] Java: `SuperpositionError` has no `toString()` → renders class-name, not `"{CODE}: {message}"` (§3.1 SHOULD).
- [ ] Java: RefreshStrategy defaults (30000/60000/300000) not encoded; no default-OnDemand when none configured (§9).
- [ ] Java: OnDemand + direct post-shutdown eval can throw `RejectedExecutionException` (unchecked) instead of clean `PROVIDER_ERROR` (§12.3 edge).
- [ ] Java: out-of-int32-range integral values rejected as TYPE_MISMATCH (platform-forced by 32-bit `Integer`; documented, borderline).

### 0c. Confirmed clean (spec-compliant, no action)
Type predicates (bool≠int, object accepts array, float widens int, no string
coercion), §3.2 error ordering, candidate=EXACT/matching=SUBSET, mtime `<=`,
TTL-off-checkedAt incl. 304, config-error-wins, STALE-on-any-failure, exact
init error strings (except Rust `EXP-INIT-STR`), single-shot guard, fallback
init-only, and the canonical contract names (§1.4) — **PASS in all four**.

---

## 1. MED review findings (behavioral divergences)

IDs in the last column map to the "Known Inconsistencies" table in
`clients/PROVIDER_SPEC.md` §20 (canonical behavior defined there).

| # | Sev | Finding | Languages affected | Spec ID |
|---|-----|---------|--------------------|---------|
| ~~1~~ ✅ | MED | ~~`getApplicableVariants` swallows API errors → returns `[]`~~ **DONE** — Python + JS remote now propagate as `NETWORK_ERROR` (match Rust `map_err(NetworkError)?` / Java); empty-success still returns `[]` | Python, JS | `VAR-ERR` |
| ~~2~~ ✅ | MED | ~~Background refresh loop keeps a **strong** `Arc`/ref → provider never dropped~~ **DONE** — `start_polling`/`start_watching` now capture `Weak<Inner>` (mirroring `do_refresh`) and upgrade per tick; polling `break`s when upgrade → `None`, watch `break`s on that **and** on `recv()` → `Closed` (also fixes a busy-spin on the closed channel). A dropped provider now lets `Inner` reach refcount 0 → RAII `Drop` releases the task, data sources, and the `notify` OS watcher — matching the weakref approach in Python/JS/Java. (Rust has no FFI `ProviderCache`; the leaked resources were the task + data sources, not a native cache.) **RUNTIME-VERIFIED:** added `leak_tests` (server-free unit tests) — `polling_loop_does_not_pin_the_provider` + `watch_loop_does_not_pin_the_provider` assert `Arc::strong_count == 1` after init (the task holds only `Weak`) and `weak.upgrade().is_none()` after drop; both pass, and confirmed non-vacuous (reintroducing the strong `self.clone()` makes the poll test fail with count 2). | Rust (`start_polling`/`start_watching`) | `RS-LEAK` |
| ~~3~~ ✅ | MED | ~~Empty/absent local targeting key → `[]` vs `""`~~ **DONE** — Python (`targeting_key or ""`) + JS (`targetingKey ?? ""`) now hand `""` to the core instead of short-circuiting; matches Rust/Java and their own remote providers | Python, JS | `VAR-KEY` |
| ~~4~~ ✅ | MED | ~~Constructor does not validate options~~ **DONE** — Rust `SuperpositionOptions::new -> Result<Self>` validates (6 callers `.expect`); Python `__post_init__` raises `ValueError` on blank endpoint/auth/org/workspace. Matches Java/JS | Rust, Python | `OPT-VAL` |
| ~~5~~ ✅ | MED | ~~File watch observes the file **node** not its dir~~ **DONE** — Rust now watches the parent dir (NonRecursive) and filters events by the target filename; survives atomic-rename saves, matching Java/JS | Rust | `FS-WATCH` |
| ~~5b~~ ✅ | MED | ~~Python watches the dir but the handler only overrides `on_modified`~~ **DONE** — `_FileEventHandler` watched the parent dir yet reacted to `on_modified` **only**, so atomic-rename saves (delivered as create/move against the dir entry) were dropped — it silently defeated the dir-watch. Now handles created/modified/moved and checks both `src_path` and `dest_path` (a rename's target is `dest_path`). | Python | `FS-WATCH` |
| ~~6~~ ✅ | MED | ~~Remote-provider shutdown/teardown divergence — no post-shutdown guard~~ **DONE** (== `REMOTE-SHUT` §0a) | Python | — |
| ~~7~~ ➖ | MED | ~~Object accessor rejects top-level JSON arrays~~ **N/A (platform)** — OpenFeature Rust's object method returns `StructValue` (map-only, no array form) and there's no `resolve_array_value`; `TYPE_MISMATCH` is correct. The index-keyed-map hack was deliberately removed (`conversions.rs:296`); `resolve_array` is the direct escape hatch. Accepted platform characteristic | Rust | `RS-OBJ` |
| ~~8~~ ➖ | MED | ~~Integer resolve accepts floating values (`1.5` → int)~~ **N/A (platform)** — OpenFeature JS has a single `number` type / only `resolveNumberEvaluation` (no `resolveIntegerEvaluation`); the provider can't know an integer was requested, and `asNumber` accepting `1.5` is correct. A `Number.isInteger` check would wrongly reject every float read. Accepted platform characteristic (bucket with `RS-EVENTS`/`JS-SERR`) | JS | `J-INT` |
| 9 | MED | Sync `resolve` path skips `ensureFreshData` (no OnDemand refresh on sync calls) | Python | `PY-SYNC` |
| 10 | LOW | `ErrorCode` omits `SERIALIZATION_ERROR` | JS | `JS-SERR` (intentional) |

## 1a. Remove deprecated legacy providers

The old pre-architecture providers are **wrong and deprecated** — they are not to
be reconciled, only deleted. They are explicitly out of scope in the spec (§1).

- [ ] Delete Rust legacy `SuperpositionProvider` (`provider.rs`) + its `CacConfig`/`ExperimentationConfig` client path (`client.rs`) once nothing depends on it.
- [ ] Delete Python legacy `SuperpositionProvider` (`provider.py`) + `configuration_client.py` / `exp_config.py` / `cac_config.py` as appropriate.
- [ ] Delete JS legacy `SuperpositionProvider` (`superposition-provider.ts`) + `configuration-client.ts` / `experimentation-client.ts` + the lenient `utils.ts` coercion helpers.
- [ ] Delete Java legacy `SuperpositionOpenFeatureProvider` + `RefreshJob` / `SuperpositionConfig` if superseded.
- [ ] Repoint any CI / provider-sdk-tests still on a legacy provider to `LocalResolutionProvider` (see §6), then drop the legacy exports from each package `index`.

## 2. LOW review findings

- [ ] JS: `fetchedAt = now()` instead of server `last_modified` in one path — confirm/fix.
- [ ] Java: applicable-variant strings vs structured — cosmetic.
- [x] ~~Deprecated `*_seconds` fields on refresh strategies — remove where safe~~ **DONE** — removed from the two languages that carried them (Rust `types.rs`, Python `types.py`); Java/JS were always ms-only. Dropped the deprecated `interval`/`ttl`/`timeout` seconds fields + the `_resolve_ms`/`_reject_both`/`DeprecationWarning` machinery (Python) and `#[deprecated]` fields + `#[allow(deprecated)]` (Rust); `interval_milliseconds`/`ttl_milliseconds` are now required (non-Optional). Accessors (`interval_ms()`/`ttl_ms()`/`timeout_ms()`) kept as passthroughs so the legacy `client.rs`/`cac_config.py`/`exp_config.py` still work. Migrated all seconds-kwarg call sites (Python examples + `test.py`) to `_milliseconds` (×1000). Rust: lib+examples+tests build, 11 lib tests pass. Python: compiles + runtime-verified (deprecated kwargs now raise `TypeError`).
- [ ] Misleading docstrings / comments referencing old shapes — sweep. (Fold into the §8 standalone-docs sweep.)

## 3. PrefixQueryInterceptor follow-ups (deferred — "do later")

- [ ] Move `PrefixQueryInterceptor` into the **SDK** module, parallel to `client/auth/`, opt-in like auth.
- [ ] Make it schema-driven (detect list-typed `@httpQuery` members generically, not a hardcoded 4-input `instanceof` chain).
- [ ] Relocate `SdkListQuerySerializationTest` into the SDK module.
- [ ] Add an **API-provider-specific** regression test: assert `prefix`/`exclude_prefix` reach the wire for `ApplicableVariants` + `GetResolvedConfigWithIdentifier`, exactly once.
- [ ] Probe whether a smithy-java upgrade serializes list `@httpQuery` members → retire the interceptor.

## 4. Rust refactor

- [x] ~~`start_polling` / `start_watching`: replace strong `self.clone()` capture with `Weak`~~ **DONE (RS-LEAK, §1 #2).** Used `Weak` upgrade-per-tick (mirroring the existing `do_refresh` idiom), not a `CancellationToken` — the three FFI clients all use weak-ref (not a token), and Rust already has `close_provider().abort()` as the explicit-shutdown path. Watch also `break`s on `recv()` → `Closed`. `cargo build` green.

## 5. Kotlin conversion (analysis only — not a quick win)

- [ ] Convert `traits/AllFeatureProvider.java` + `FeatureExperimentMeta.java` to Kotlin. Requires `-Xjvm-default=all` in the Kotlin convention plugin (else Java implementers lose the `default` methods via `DefaultImpls`) + `@Throws`. Do as part of a fuller Kotlin migration, not in isolation.

## 6. Verification / housekeeping (before or alongside commit)

- [ ] Run full test suites — none run end-to-end yet:
  - [ ] Rust: `cargo test -p superposition_provider`
  - [ ] Python: `pytest clients/python/provider`
  - [ ] JS: `npm test` (jest) in `open-feature-provider`
  - [ ] Java: full `gradle test` (only `HttpDataSourceTest` + `SdkListQuerySerializationTest` run so far)
- [ ] Rebuild JS provider `dist/` (`node build-deps.js && npm run build`) — current bundle predates the changes.
- [x] ~~CI provider-sdk-tests: point each language's harness at the **new** provider~~ **DONE** — JS (`index.js`), Python (`main.py`), Rust (`integration_test.rs`) were already new-only. Java (`Main.kt`): legacy `SuperpositionOpenFeatureProvider`/`SuperpositionProviderOptions` imports dropped. (Legacy provider *classes* still in the module — see §1a.)
- [x] ~~Harness parity + coverage refactor~~ **DONE** — all four harnesses now run a **shared scenario-runner** over the **same four flows**: **A** Local+HTTP (Polling), **B** Remote `SuperpositionAPIProvider`, **C** Local+wrong-HTTP→**file fallback** (no experiments), **D** Local+HTTP (**OnDemand** — previously untested anywhere). De-duplicated the 3× copy-paste: Rust `run_scenarios`/`run_flow` (1013→615), Python `run_provider_tests(..., is_async)` (704→499), Java `runProviderTests` + `runDemo` repurposed to Flow D (752→622); JS `runProviderTests` grew to 4 flows (+ new `config.toml`). Compiles/typechecks in all four (cargo `--no-run`, py_compile, `node --check`, compileKotlin).
- [x] **BUG FOUND + FIXED (Python harness):** `main.py` constructed `SuperpositionOptions(token=...)` but the type takes `auth: AuthMethod` (both HEAD and working tree) — a latent `TypeError`, so the Python harness could never have run against the current provider. Fixed to `auth=TokenAuth(...)`.
- [ ] Commit `provider/updated` once suites are green.

## 7. Spec

- [x] `clients/PROVIDER_SPEC.md` — language-agnostic behavioral spec (see that file). Any inconsistencies found while writing it are logged in its "Known Inconsistencies" section and should feed back into sections 1–2 above.
- [x] ~~Reconcile spec §20 stale "fixed" entries~~ **DONE — §20 removed entirely.** A "Known Inconsistencies" ledger of per-language deviations/status never belonged in a canonical, standalone spec (it made the contract depend on the current implementations — the opposite of the goal). Deleted §20 and cleaned up all 11 cross-references into it; the canonical rule already lived in each section (§4/§3.4/§11 for the genuine platform variances, stated language-neutrally). **All per-implementation deviation/status tracking lives here in `PROVIDER_TODO.md`, not the spec.** Also corrected while in there: §17 RS-LEAK is **Weak-only, no cancellation token**, and "native cache handle / FFI cache" scoped to the FFI clients (Rust releases data-sources/watcher via `Drop`); §7 sharpened to require reacting to **create/rename**, not only in-place modify; §19 checklist updated.
- [ ] **Open decisions formerly parked in spec §20, now owned here:** **PY-SYNC** (§1 MED #9) and **WATCH-FILTER** (§0b file-watch event-filter divergence — overflow-robustness + symlink handling). The spec states these as implementation-defined pending a decision; resolve them here, then encode the chosen canonical behavior in the spec body.

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
