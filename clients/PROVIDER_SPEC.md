# Superposition OpenFeature Provider — Language-Agnostic Behavioral Specification

**Status:** normative · **Version:** 1.0 · **Audience:** anyone implementing a
Superposition OpenFeature provider in a new (or existing) language.

This document defines the *observable behavior* of a Superposition OpenFeature
provider precisely enough that an implementation written **only** from this text
will be behaviorally consistent with every other conformant implementation
(Rust, Python, Java/Kotlin, JavaScript). You should not need to read another
language's source to implement a new one. Where the current implementations
disagree with each other, the canonical behavior is stated here; per-implementation
status and open questions are tracked in `clients/PROVIDER_TODO.md`, not in this
spec.

---

## 0. Conformance language & precedence

### 0.1 Requirement keywords
The keywords **MUST**, **MUST NOT**, **REQUIRED**, **SHALL**, **SHOULD**,
**SHOULD NOT**, **MAY**, and **OPTIONAL** are to be interpreted as in RFC 2119.
A conformant provider MUST satisfy every **MUST** and MUST NOT violate any
**MUST NOT**. **SHOULD**-level items are strong recommendations; deviating from
one requires a documented, deliberate reason.

### 0.2 Precedence — this spec vs the language's OpenFeature SDK (read this)
Every implementation targets a specific language whose OpenFeature SDK already
defines an idiomatic provider surface (interface/trait names, method signatures,
sync vs async, the `ErrorCode`/`Reason` enums, the provider-status model, the
event system, the `EvaluationContext`/`Value` types).

> **Rule of precedence.** For everything the target language's OpenFeature SDK
> specifies about *how a provider integrates with that SDK* — method names and
> shapes, synchronous vs asynchronous resolution, the exact `ErrorCode` and
> `Reason` value names, how provider status and events are surfaced, how the
> evaluation context and structured `Value` types are modeled — the **language's
> OpenFeature specification and SDK take precedence**. Implement idiomatically
> for that language.
>
> This document takes precedence for everything **Superposition-specific**: the
> data-source abstraction, the refresh strategies and their timing, the
> `if-modified-since`/304 and TTL semantics, the STALE machinery, the init/
> fallback rules, the type-acceptance predicates (which values are a valid
> boolean/integer/etc.), filtering, and applicable-variant resolution.
>
> When the two genuinely conflict (e.g. the language's OpenFeature SDK has **no**
> provider-event API, or models integers and doubles as one number type), follow
> the language SDK for the surface, preserve the *observable semantics* this spec
> mandates as closely as the platform allows, and **document the deviation** in
> the implementation.

In short: **map this spec's semantics onto your language's OpenFeature idioms;
never invent a surface that contradicts your language's OpenFeature SDK, and
never weaken the semantics defined here to save effort.**

> **Naming is part of the contract, not idiom.** The precedence rule above
> governs *behavior and OpenFeature-integration surface* — it does NOT license
> renaming the Superposition-specific traits, types, and methods. Those have
> canonical names ([§1.4](#14-canonical-contract-surface--the-named-contract-required))
> that every implementation MUST preserve (only casing/keyword adapts). The one
> place naming defers to the language's OpenFeature SDK is the per-type
> resolution entry points, called out explicitly in §1.4.

---

## 1. Overview & terminology

A Superposition provider resolves feature-flag values from a Superposition
workspace. Two provider archetypes exist; both implement the language's
OpenFeature `FeatureProvider` interface.

> **Scope — legacy providers are out of scope and MUST be ignored.** Some
> language packages still ship an older, pre-architecture provider (e.g. a
> `SuperpositionProvider` built directly on ad-hoc config/experiment clients).
> Those are **deprecated, behaviorally incorrect, and scheduled for removal.**
> They MUST NOT be used as a reference, their behavior MUST NOT be replicated,
> and their divergences are NOT tracked as inconsistencies in this document. A
> new implementation implements **only** the two archetypes below.

| Provider | Caching | How it resolves |
|---|---|---|
| **Local resolution provider** | Caches config + experiments locally in a native FFI cache; refreshes per a strategy | Evaluates flags **in-process** against the cache using the Superposition core (FFI) |
| **Remote / API provider** | None | Every evaluation is a **server round-trip**; the service applies contexts/overrides/experiments and returns finished config |

**Core terms:**
- **Config** — the resolved configuration document: `contexts`, `overrides`,
  `default_configs` (flag key → value), `dimensions` (dimension metadata).
- **ExperimentConfig** — `experiments` + `experiment_groups`.
- **Data source** — an abstraction that *fetches* Config / ExperimentConfig
  (HTTP, File, or a composed provider). See [§5](#5-data-source-interface).
- **Refresh strategy** — when/how the local cache is refreshed. See [§9](#9-refresh-strategies).
- **Evaluation context** — the OpenFeature per-evaluation context; carries a
  **targeting key** and arbitrary attributes (dimensions).
- **FFI cache / core** — the shared native Superposition resolution engine the
  local provider drives. This spec treats it as a black box with the operations
  in [§13](#13-local-provider-evaluation-algorithm).

### 1.4 Canonical contract surface — the named contract (REQUIRED)

A "follow-exactly" spec fixes not only behavior but the **named contract**. The
Superposition-specific types, traits/interfaces, and methods below are part of
the contract: every implementation MUST expose them **under these names**,
adapting only (a) the language's case convention (`PascalCase` for types;
`camelCase` vs `snake_case` for members) and (b) the language's keyword for the
construct (`trait` / `interface` / `abstract class` / ABC / sealed type).
Implementers MUST NOT rename, merge, or split these. This is what makes an
implementation recognizably the *same* provider rather than a lookalike.

**Types & errors**

| Canonical name | Construct | Members / variants |
|---|---|---|
| `SuperpositionError` | error type | carries an `ErrorCode`; SHOULD render `"{CODE}: {message}"` |
| `ErrorCode` | enum | `CONFIG_ERROR`, `NETWORK_ERROR`, `SERIALIZATION_ERROR`, `PROVIDER_ERROR`, `DATA_SOURCE_ERROR`, `REFRESH_ERROR` ([§3.1](#31-superposition-error-type--codes-required)) |
| `FetchResponse<T>` | sum type | `Data(T)` \| `NotModified`; `data()`, `notModified()`, `isNotModified()`, `getData()`, `mapData()` ([§2.1](#21-fetchresponset--a-sum-type-required)) |
| `ConfigData` | struct/record | `{ data: Config, fetchedAt }` |
| `ExperimentData` | struct/record | `{ data: ExperimentConfig, fetchedAt }` |
| `SuperpositionOptions` | struct | `validate()` ([§18](#18-options--validation)) |
| `AuthMethod` | sum type | `Token` \| `Basic` |
| `RefreshStrategy` | sum type | `Polling` \| `OnDemand` \| `Watch` \| `Manual` ([§9](#9-refresh-strategies)) |

**Data source** — `SuperpositionDataSource` (trait/interface) with **exactly**
these methods (case-adapted), signatures per [§5](#5-data-source-interface):
`fetchConfig`, `fetchFilteredConfig`, `fetchActiveExperiments`,
`fetchCandidateActiveExperiments`, `fetchMatchingActiveExperiments`,
`supportsExperiments`, `watch`, `close`. Concrete implementations:
**`HttpDataSource`**, **`FileDataSource`**.

**Resolution traits**
- **`AllFeatureProvider`** — hosts the type-coercion contract:
  `resolveAllFeatures`, `resolveAllFeaturesWithFilter`, `resolveTyped`.
- **`FeatureExperimentMeta`** — `getApplicableVariants`.

**Providers** — the two archetypes ([§1](#1-overview--terminology)):
**`LocalResolutionProvider`** and **`SuperpositionAPIProvider`**.

**The per-type resolution entry points follow the language's OpenFeature SDK
(the one place naming defers, per [§0.2](#02-precedence--this-spec-vs-the-languages-openfeature-sdk-read-this)).**
The methods invoked by the SDK's `getBooleanEvaluation` / `getStringEvaluation` /
`getIntegerEvaluation` / `getDoubleEvaluation` / `getObjectEvaluation` (e.g.
`resolveBool`/`resolveString`/`resolveInt`/`resolveFloat`/`resolveStruct`, or
`resolve{Type}Evaluation`, or `resolve_int`/`resolve_object`, …) MUST be named to
match that SDK's typed-method surface — but each MUST apply the correct
[§4](#4-type-coercion-contract) predicate. In particular, **if the SDK exposes a
single number method, that method MUST still enforce the integer predicate when
an integer resolve is requested** (this is the `J-INT` rule — do not let the
shared number path accept a float as an integer).

An implementation MAY add non-contract helpers (e.g. an abstract `BaseDataSource`
supplying the `fetchConfig` default) — these are implementation details and are
not part of the named contract, but they MUST NOT replace or rename the canonical
names above.

---

## 2. Data model

### 2.1 `FetchResponse<T>` — a sum type (REQUIRED)
Fetches MUST return a two-state value, **not** "`T` or null":

```
FetchResponse<T> = Data(value: T) | NotModified
```

- `FetchResponse.data(v)` / `FetchResponse.notModified()` constructors.
- `isNotModified() -> bool`
- `getData() -> T?` — returns the value for `Data`, **absent/null** for `NotModified`.
- `mapData(fn)` — maps the `Data` arm; passes `NotModified` through unchanged.

`NotModified` MUST be distinguishable from `Data` carrying an empty payload — a
304 (unchanged) is semantically different from "the source returned nothing."

### 2.2 `ConfigData` / `ExperimentData` (REQUIRED)
```
ConfigData     { data: Config,           fetchedAt: Timestamp }
ExperimentData { data: ExperimentConfig, fetchedAt: Timestamp }
```

**`fetchedAt` semantics (REQUIRED — this is the pivot of the 304 mechanism):**
- For an **HTTP** source, `fetchedAt` MUST be the **server's `last_modified`**
  timestamp from the response (falling back to "now" only if the server omits
  it). It is *server modification time*, not wall-clock fetch time.
- For a **File** source, `fetchedAt` MUST be the **local read time** ("now",
  captured at read). (It is NOT the file mtime; the file's own if-modified check
  uses mtime separately — see [§7](#7-file-data-source).)
- `fetchedAt` from the last successful fetch MUST be sent back as the next
  fetch's `if-modified-since` ([§10](#10-refresh-execution)).

### 2.3 `Config` shape (as consumed)
`Config` MUST expose at least: `default_configs` (flag key → resolved value),
and `dimensions` (dimension metadata used for experiment matching). Values in
`default_configs` are the resolved flag values the type-coercion contract
([§4](#4-type-coercion-contract)) operates on.

---

## 3. Error model

### 3.1 Superposition error type & codes (REQUIRED)
Provider-internal failures MUST be represented by a single Superposition error
type carrying one of these codes (names MAY be adapted to the language's casing
conventions, but the **set and meaning are fixed**):

| Code | Meaning / canonical raise conditions |
|---|---|
| `CONFIG_ERROR` | Initialization failures (config/experiment fetch failed with no viable fallback); local evaluation failures from the core; "watch selected but source can't watch." |
| `NETWORK_ERROR` | Any non-304 transport/SDK failure from an HTTP fetch or a remote resolve/applicable-variants call. |
| `SERIALIZATION_ERROR` | A value returned by the core is not well-formed for its declared type (e.g. a flag value that cannot be decoded). **See [§3.4](#34-serialization_error-and-language-variance).** |
| `PROVIDER_ERROR` | Operating on an uninitialized/shut-down provider (e.g. resolve with no cached config; use after shutdown). |
| `DATA_SOURCE_ERROR` | File source errors (bad extension, read/parse/stat failure); "experiments not supported by this source"; a local-provider-as-data-source with no cache. |
| `REFRESH_ERROR` | A refresh exceeded its timeout (or was interrupted). |

The error type SHOULD carry an optional `cause`/wrapped exception and render as
`"{CODE}: {message}"`.

### 3.2 OpenFeature evaluation error mapping (REQUIRED)
On the typed-resolution path the provider MUST return the language's OpenFeature
`FlagResolutionDetails`/`ProviderEvaluation` with these error codes — it MUST NOT
throw these to the caller (the OpenFeature SDK owns that). Given a resolved
config map and a requested `flagKey` + `typeName`:

1. **Key absent** from the resolved map → OpenFeature **`FLAG_NOT_FOUND`**,
   reason `ERROR`, value = caller default, message `"Flag '{key}' not found"`.
2. **Key present but the type predicate rejects the value** → OpenFeature
   **`TYPE_MISMATCH`**, reason `ERROR`, value = default, message
   `"Flag '{key}' is not a {typeName}"`.
3. **Any other error** thrown while producing the resolved map (network, core
   failure, provider-not-initialized, serialization) → OpenFeature **`GENERAL`**,
   reason `ERROR`, value = default, message
   `"Error evaluating flag '{key}': {underlying message}"`.

The evaluation order MUST be: (1) not-found, then (2) type check, then success;
any thrown error routes to (3). These three are the **only** OpenFeature error
codes a conformant provider emits. (`TARGETING_KEY_MISSING`, `PARSE_ERROR`,
etc. MUST NOT be produced.)

### 3.3 Success result shape (REQUIRED / SHOULD)
On success the provider MUST return the extracted value. It **SHOULD leave
`reason` unset** on success (the core cannot yet report per-key provenance such
as STATIC vs TARGETING_MATCH vs SPLIT; emitting a guessed reason would be
wrong). A `variant` string MAY be set (advisory; not observable-behavior-
critical).

### 3.4 `SERIALIZATION_ERROR` and language variance
`SERIALIZATION_ERROR` exists for implementations where the core hands back flag
values as **encoded strings** that the provider must decode per key (so a single
malformed value can be attributed to its flag). If, in the target language, the
FFI/core returns **already-typed native values** (no per-key decode step), the
provider MAY omit `SERIALIZATION_ERROR` as a deliberate, documented choice — a
decode error simply cannot arise. Decode errors, where they can occur, MUST be
per-key (name the offending flag), not fail the whole evaluation.

---

## 4. Type-coercion contract

This is the **most consistency-critical** section. When resolving a flag of a
requested type, the provider applies a **type predicate** to the raw value from
the resolved config. If the predicate accepts, the (possibly widened) value is
returned; if it rejects, the result is `TYPE_MISMATCH` ([§3.2](#32-openfeature-evaluation-error-mapping)).

**There is NO lenient coercion.** Strings are not parsed into numbers/booleans;
`"true"` is not a boolean; `1` is not a boolean; `0`/`1` are not booleans. The
predicates below are exhaustive.

| Requested type | Accepts iff the value is… | Rejects (→ TYPE_MISMATCH) | Widening |
|---|---|---|---|
| **boolean** | a boolean | numbers, strings, `1`/`0`, `"true"` | none |
| **string** | a string | numbers, booleans, objects | none |
| **integer** | a number that is **integral** (no fractional part) **and not a boolean** | booleans; floats with a fractional part (`1.5`); a value stored/typed as floating even if whole (`2.0`) — see note | none |
| **float / double** | a number **and not a boolean** | booleans, strings | **integer widens to float** (`10` → `10.0`) |
| **object / struct** | a non-null object **or** an array | scalars (bool/number/string), null | none |

**Detailed rules (REQUIRED):**
- **boolean:** accept only a genuine boolean. A boolean MUST NOT be accepted as
  an integer or a float, and an integer MUST NOT be accepted as a boolean.
- **integer:** accept only values whose runtime/JSON type is an integer number.
  A floating value MUST be rejected even if mathematically whole. (Rationale:
  the majority of implementations key off the value's *representation*; `2.0`
  arrives as a float and is rejected. Implementations MUST NOT accept arbitrary
  floats as integers.)
- **float/double:** accept any number (integer or floating), widening integers
  losslessly to floating. Reject booleans and strings.
- **object:** accept a non-null map/object **or** a top-level array. Reject
  scalars and null. (OpenFeature has no dedicated array type; arrays are resolved
  through the object/struct path.) **Platform exception:** this holds only where
  the language's OpenFeature object type can *represent* an array (`JsonValue` /
  `dict`|`list` / `Value`). Where the object type is map-only — Rust's
  `StructValue` — a top-level array cannot be returned, so the object method MUST
  return `TYPE_MISMATCH` rather than fabricate an index-keyed map; a separate
  array accessor (e.g. Rust `resolve_array`) is the escape hatch.
- **null handling inside structures:** a null-valued *field* of an object SHOULD
  be dropped rather than failing the whole object. A null *element* of an array
  MAY be treated as an error for that value (implementation-consistent; document
  the choice).

If the target language's OpenFeature SDK exposes **separate** integer and double
resolve methods, the provider MUST honor the distinct predicates above (integer
rejects floats; double accepts both). If the language models a **single** number
type, it MUST still apply the integer predicate when the OpenFeature integer
method is invoked and the float predicate for the double method — do not collapse
them.

---

## 5. Data source interface

A data source fetches Config and ExperimentConfig. The interface is
**`SuperpositionDataSource`** and its method names are canonical
([§1.4](#14-canonical-contract-surface--the-named-contract-required)) — only
casing adapts; **parameter order and semantics are fixed**:

```
interface SuperpositionDataSource {
  // Config
  fetchConfig(ifModifiedSince?: Timestamp) -> FetchResponse<ConfigData>
      // DEFAULT: delegates to fetchFilteredConfig(none, none, none, ifModifiedSince)
  fetchFilteredConfig(
      context?: Map<String,String>,       // dimension values, encoded
      prefixFilter?: List<String>,
      excludePrefixFilter?: List<String>,
      ifModifiedSince?: Timestamp) -> FetchResponse<ConfigData>

  // Experiments
  fetchActiveExperiments(ifModifiedSince?: Timestamp) -> FetchResponse<ExperimentData>
  fetchCandidateActiveExperiments(
      context?, prefixFilter?, excludePrefixFilter?, ifModifiedSince?) -> FetchResponse<ExperimentData>
  fetchMatchingActiveExperiments(
      context?, prefixFilter?, excludePrefixFilter?, ifModifiedSince?) -> FetchResponse<ExperimentData>

  supportsExperiments() -> bool          // DEFAULT: false
  watch() -> Stream<changePath>? | null  // DEFAULT: none/null (no watching)
  close()
}
```

**Canonical parameter order (REQUIRED) everywhere filters appear:**
`(context, prefixFilter, excludePrefixFilter, ifModifiedSince [, dimensionMatchStrategy])`.

- `supportsExperiments()`: HTTP source → `true`; File source → `false`.
- `candidate` vs `matching` experiments map to a dimension-match strategy — see
  [§6.3](#63-experiments--dimension-match-strategy).
- `close()` MUST be idempotent and release resources (connections, watchers).

---

## 6. HTTP data source

Backed by the Superposition SDK client for the language.

### 6.1 Config fetch
Issue the "get config" command with `workspace_id`, `org_id`, and:
- `context` — **only if non-empty** (dimension map, values encoded so numbers/
  booleans reach the service typed, not stringified).
- `prefix` — only if the list is non-empty.
- `exclude_prefix` — only if the list is non-empty.
- `if_modified_since` — if provided.

On success build `ConfigData{ data: <config from response>, fetchedAt:
response.last_modified }`.

### 6.2 `if-modified-since` / 304 (REQUIRED)
The service returns **HTTP 304** when the config/experiments have not changed
since `if_modified_since`. Many SDKs surface a 304 as an *error* (no modeled 304
shape). The provider MUST detect 304 — by inspecting the raw response status
(e.g. via an interceptor/middleware, or the error's status metadata) — and return
`FetchResponse.notModified()`. Any **non-304** failure MUST become a
`NETWORK_ERROR`. A 304 MUST NOT be treated as an error.

### 6.3 Experiments & dimension-match strategy (REQUIRED)
Experiment fetches use a "get experiment config" command with the same
filter/if-modified-since threading, plus a **dimension match strategy**:

| Method | Strategy sent |
|---|---|
| `fetchActiveExperiments` | **none** (no strategy field) |
| `fetchCandidateActiveExperiments` | **EXACT** |
| `fetchMatchingActiveExperiments` | **SUBSET** |

So **candidate = EXACT**, **matching = SUBSET**.

### 6.4 List-typed query parameters (implementation note)
`prefix` and `exclude_prefix` are **list-typed** query parameters. Some generated
SDKs fail to serialize list-typed query members onto the request URI, silently
dropping the filters so responses come back unfiltered. A new implementation MUST
**verify** its SDK actually serializes list `@httpQuery` members (a wire-level
test) — do not assume. If the SDK has this defect, the provider MUST compensate
(e.g. an interceptor that emits repeated `key=v` params, or comma-joins onto the
query string as the service parses) and MUST register that compensation on
**every** client that sends these filters — both the HTTP data source (config +
experiments) and the remote/API provider (resolved-config + applicable-variants).
The workaround MUST be covered by a regression test and removed if/when the SDK
is fixed.

> **Verified status (2026-07):** among the current SDKs, only **smithy-java**
> (`0.0.1`, pre-release) has this defect — it is compensated by
> `PrefixQueryInterceptor` and guarded by a regression test. The **aws-smithy-rust**,
> **smithy-python**, and **smithy-typescript (restJson1)** clients all serialize
> list query members correctly (each loops the list into repeated `prefix=` /
> `exclude_prefix=` params), so no compensation is needed there. Re-verify on any
> SDK upgrade.

---

## 7. File data source

Loads Config from a local file. **Experiments are NOT supported** by a file
source: `supportsExperiments()` → `false`, and all three experiment fetches MUST
fail with `DATA_SOURCE_ERROR` "Experiments not supported by FileDataSource".

- **Format:** chosen by the file extension (case-insensitive): `.json` and
  `.toml` are supported. A missing extension or an unsupported extension MUST
  raise `DATA_SOURCE_ERROR` at construction.
- **Parse + filter:** read the whole file and parse via the core, applying the
  `context` (dimension), `prefix`, and `exclude_prefix` filters **at parse time**
  (same filtering the server would apply). Parse failure → `DATA_SOURCE_ERROR`.
- **`fetchedAt`:** the local **read time** ("now"), captured at read
  ([§2.2](#22-configdata--experimentdata-required)).
- **if-modified-since via mtime (REQUIRED):** compute the file's modification
  time. The file is **NotModified** iff `mtime <= ifModifiedSince` (a `<=`
  comparison — equal timestamps count as unchanged). When `ifModifiedSince` is
  provided and the file is not modified, return `FetchResponse.notModified()`;
  otherwise re-read.
- **watch() (REQUIRED shape):** returns a stream that emits when the target file
  changes. The watcher MUST observe the file's **parent directory** and filter
  events to the target filename (watching the directory, not the file node
  directly, is required so atomic-rename saves — common in editors — are
  detected). It MUST react to **create/rename** events on the target, not only
  in-place modifications: an atomic-rename save surfaces as a create/move of the
  target (typically the moved-*to* path), so a watcher that handles "modified"
  alone will miss it. Multiple `watch()` callers SHOULD share a single OS watcher;
  the watcher is torn down when the last subscriber leaves. **Debouncing is NOT
  done in the source** — it is applied by the provider's Watch strategy
  ([§9.3](#93-watch)). How events are matched to the target — and how
  dropped-event/overflow and symlinked paths are handled — is currently
  implementation-defined, pending a decision tracked in `clients/PROVIDER_TODO.md`.

---

## 8. Fallback composition (REQUIRED)

A local provider MAY be configured with a **primary** and an optional **fallback**
data source.

- The fallback is consulted **only during initialization**
  ([§12](#12-provider-lifecycle)) — never during subsequent refreshes. After
  init, refreshes use the **primary only**; if the primary later fails, the
  provider goes STALE and the fallback is NOT used to recover.
- **Config init:** try primary; on failure, if a fallback exists, try it. Error
  messages are fixed strings (see [§12.2](#122-initialization-algorithm)).
- **Experiment init:** the fallback is only usable for experiments if it **also**
  `supportsExperiments()`.

---

## 9. Refresh strategies

Four strategies. **All durations are in milliseconds.** Implementations SHOULD
NOT introduce seconds-based fields.

| Strategy | Fields (all ms) | Defaults |
|---|---|---|
| **Polling** | `intervalMilliseconds`, `timeoutMilliseconds` | interval **60_000**, timeout **30_000** |
| **OnDemand** | `ttlMilliseconds`, `useStaleOnError`, `timeoutMilliseconds` | ttl **300_000**, `useStaleOnError` **true**, timeout **30_000** |
| **Watch** | `debounceMs` | debounce **500** |
| **Manual** | *(none)* | — |

**Only Polling and OnDemand carry a `timeoutMilliseconds`.** Watch and Manual have
no timeout — each of their refreshes runs unbounded ([§10.5](#105-timeout)). A
`Manual` strategy carries no configuration at all.

**Default strategy when none is configured: OnDemand** (with the defaults above).

### 9.1 Polling
Run a background loop that **sleeps `interval` first, then refreshes**, forever
(init already performed the initial fetch, so the first poll happens one interval
after init). Each tick runs the full [refresh](#10-refresh-execution). Refresh
errors are logged and the loop continues (they still drive STALE via
[§11](#11-stale-state--events)).

### 9.2 OnDemand
No background loop. Freshness is enforced **lazily on each evaluation** via the
TTL clock ([§10.3](#103-ondemand-ttl--the-checkedat-clock)).

### 9.3 Watch
Subscribe to `primary.watch()`. If the primary cannot watch (`watch()` returns
none), initialization MUST fail with `CONFIG_ERROR` "Watch strategy selected but
data source does not support watching". On each change event, **debounce** by
`debounceMs` (coalesce a burst of events into a single refresh once the window
settles), then refresh. Only Polling and OnDemand supply a refresh timeout; Watch
and Manual run each refresh unbounded.

### 9.4 Manual
No background activity and no lazy refresh. The cache changes only when the
application explicitly calls the provider's public `refresh()`.

---

## 10. Refresh execution

`refreshOnce` (one refresh cycle):

### 10.1 Parallelism (REQUIRED)
Config and experiments MUST be refreshed **concurrently**, not sequentially
(wall-clock cost = max of the two, not the sum). Use the language's concurrency
primitive (async gather / futures / threads).

### 10.2 Per-leg behavior
- **Config leg:** `ifModifiedSince = cachedConfig?.fetchedAt`; call
  `primary.fetchConfig(ifModifiedSince)`.
  - `Data` → replace cached config, update the FFI cache, advance
    `configCheckedAt = now`.
  - `NotModified` (304) → **do not** touch the cache; still advance
    `configCheckedAt = now`.
  - error → **keep last known good** (do not clear the cache); record the error.
- **Experiment leg:** skip entirely (success, no-op) if
  `!primary.supportsExperiments()`. Otherwise same shape against
  `fetchActiveExperiments`, advancing `experimentsCheckedAt`.

`checkedAt` clocks are advanced on **every successful check including a 304**, and
are **not** advanced on error.

### 10.3 OnDemand TTL — the `checkedAt` clock (REQUIRED)
The OnDemand TTL MUST be measured off the local `configCheckedAt` /
`experimentsCheckedAt` timestamps — **not** off the server's `last_modified` /
`fetchedAt`. On each evaluation ("ensure fresh"):
- `shouldRefreshConfig` = `configCheckedAt == null` OR
  `(now - configCheckedAt) > ttl` (strict `>`).
- `shouldRefreshExperiments` = `supportsExperiments()` AND
  (`experimentsCheckedAt == null` OR `(now - experimentsCheckedAt) > ttl`).
- If either is true, run a refresh. On refresh failure: if `useStaleOnError` is
  false, propagate the error out of the evaluation; if true (default), log and
  serve the stale cache.

Because a **304 advances the clock**, a stable config that keeps returning 304
resets its TTL window and is not re-fetched on every evaluation — this is the
explicit reason the TTL is decoupled from `fetchedAt` (server modification time).

### 10.4 Error priority (REQUIRED)
When both legs fail, `refreshOnce` MUST surface the **config error** (config wins);
the experiment error is surfaced only when config succeeded. Any single leg error
makes the whole refresh a failure.

### 10.5 Timeout
`refresh()` MUST bound `refreshOnce` by the strategy's `timeoutMilliseconds`
(Polling/OnDemand only; Watch/Manual unbounded). A timeout produces a
`REFRESH_ERROR` and counts as a refresh failure. The public `refresh()` MUST be
callable manually under any strategy.

---

## 11. STALE state & events

A refresh outcome drives the provider status:

- **Any** refresh failure (config OR experiment leg, OR a timeout) while the
  provider is `READY` → transition to **STALE**.
- A fully-successful refresh while `STALE` → transition back to **READY**.
- STALE is reachable **only** from READY. A refresh failure *during
  initialization* yields `ERROR`, not STALE (there is no good data to serve). A
  shut-down provider is `NOT_READY`.

**Events (REQUIRED where the language's OpenFeature SDK supports them):** on
READY→STALE emit the OpenFeature **Stale** event; on STALE→READY emit the
**Ready** event. This is required because OpenFeature SDKs typically keep their
**own** copy of provider status and do not re-read the provider's field — without
the event the SDK never learns the provider went stale/recovered.

**If the language's OpenFeature SDK has no event API** (a real platform
limitation): the provider MUST still maintain the STALE/READY status internally
so `status()`/`getState()` reflects it, and MUST document that events cannot be
emitted. This is an accepted, spec-sanctioned deviation ([§0.2](#02-precedence--this-spec-vs-the-languages-openfeature-sdk-read-this)).

**Evaluation is unaffected by STALE:** a STALE provider keeps resolving from its
last-known-good cache. Only `NOT_READY`/fatal states short-circuit evaluation
(per the language's OpenFeature SDK).

---

## 12. Provider lifecycle

### 12.1 States
`NOT_READY` (initial / after shutdown), `READY` (init succeeded), `STALE`
(refresh failing but serving cache), `ERROR` (init failed). Use the language's
OpenFeature provider-status enum.

Transitions: `NOT_READY → READY` (init ok) or `→ ERROR` (init fail);
`READY ↔ STALE` (per [§11](#11-stale-state--events)); any `→ NOT_READY`
(shutdown).

### 12.2 Initialization algorithm (local provider) (REQUIRED)
`initialize(context)`:

1. **Single-shot guard.** If current status is `READY` or `STALE`, log a warning
   and **return without re-initializing** (a live provider holds a running
   background task; re-init would orphan it). Proceed only from `NOT_READY` /
   `ERROR`.
2. Set status `NOT_READY`; store `globalContext = context` (default to empty if
   null); create a fresh FFI cache.
3. **Config fetch — REQUIRED to yield data or init fails.** Call
   `primary.fetchConfig()`. On failure:
   - No fallback → fail with `CONFIG_ERROR`:
     `"Primary config fetch failed and no fallback configured: {primaryErr}"`.
   - Fallback present → try `fallback.fetchConfig()`; if it also fails →
     `CONFIG_ERROR`:
     `"Both primary and fallback config fetch failed. Primary: {primaryErr}. Fallback: {fallbackErr}"`.
   Cache the obtained config; set `configCheckedAt = now`.
4. **Experiment fetch — conditional.** Only if `primary.supportsExperiments()`.
   If supported, experiments are **also REQUIRED** at init:
   - primary fails, no experiment-capable fallback → `CONFIG_ERROR`:
     `"Primary experiment fetch failed and no experiment-capable fallback configured: {primaryErr}"`.
   - primary fails, fallback (which MUST `supportsExperiments()`) also fails →
     `CONFIG_ERROR`:
     `"Both primary and fallback experiment fetch failed. Primary: {primaryErr}. Fallback: {fallbackErr}"`.
   Set `experimentsCheckedAt = now`. If the primary does **not** support
   experiments, skip this step entirely (non-fatal; `experimentsCheckedAt`
   stays unset).
5. Start the refresh strategy ([§9](#9-refresh-strategies)).
6. Set status `READY` (emit the OpenFeature Ready event where supported).
7. On **any** exception in steps 2–6: set status `ERROR` (emit Error event where
   supported) and propagate.

> **Design note (init "best effort"):** the fallback exists so init can *succeed*
> when the primary is down. But init MUST still end with usable data: config is
> always required, and experiments are required **when the primary claims to
> support them** — a non-experiment fallback does not satisfy an experiment-
> capable primary.

The **remote/API provider** does not fetch during init: it stores `globalContext`
and sets `READY`. It has no cache, no STALE, and MAY be re-initialized freely.

### 12.3 Shutdown (REQUIRED)
`shutdown()` (aliased by the language's `close`/`onClose` as appropriate) MUST:
stop background tasks (polling loop, watch loop/thread, debounce timers); close
the primary and fallback sources (log, do not fail, on error); free the FFI
cache; clear cached config/experiments and both `checkedAt` clocks; reset
`globalContext`; set status `NOT_READY`. It MUST be idempotent. After shutdown,
evaluation MUST fail cleanly with `PROVIDER_ERROR` ("Provider not initialized:
no cached config available"), **not** crash.

---

## 13. Local provider evaluation algorithm

For a typed resolve of `flagKey` with evaluation `context`:

1. **Freshness:** if the strategy is OnDemand, run "ensure fresh"
   ([§10.3](#103-ondemand-ttl--the-checkedat-clock)) first. (Other strategies:
   no-op.) If the language exposes both sync and async resolution, **both paths
   MUST honor freshness identically** — do not skip the OnDemand refresh on the
   sync path. (Where a language's SDK offers only a synchronous resolve path while
   the refresh is async, the implementation MUST either bridge to it or document
   the limitation.)
2. **Guard:** if there is no FFI cache / no cached config → `PROVIDER_ERROR`
   "Provider not initialized: no cached config available".
3. **Merge context:** merge `globalContext` (defaults) with the per-call
   `context` (per-call **wins** on conflict). Extract the targeting key and the
   dimension attributes ("query data").
4. **Applicable variants:** compute applicable experiment variant IDs
   ([§15](#15-applicable-variants)) and inject them into the query data (so
   experiment overrides participate) — only when experiments are cached.
5. **Evaluate:** call the core's evaluate with `(queryData, mergeStrategy,
   prefixFilter, excludePrefixFilter, targetingKey)` to obtain the resolved flag
   map. Core failure → `CONFIG_ERROR` "Failed to evaluate config: …".
6. **Extract & coerce:** apply the type contract ([§4](#4-type-coercion-contract))
   and produce the OpenFeature result per [§3.2](#32-openfeature-evaluation-error-mapping).

The "resolve all features" surface applies the same steps and returns the whole
resolved map (used by the OpenFeature typed methods).

---

## 14. Remote / API provider

Every evaluation is a live server call — no cache, no refresh, no STALE.

- **Resolve:** issue "get resolved config with identifier" with `workspace_id`,
  `org_id`, the merged `context`, `prefix`, `exclude_prefix`, and the targeting
  key as `identifier`. Set `identifier` only when the targeting key is present
  and non-empty (omit an empty/absent key). Transport failure → `NETWORK_ERROR`.
- **Response shape:** if the returned config is a non-null, non-array object, use
  it directly. Otherwise (a scalar or array top-level response) wrap it as
  `{ "_value": <response> }` so the type contract has a map to resolve against
  (the value is then reachable via the flag key `_value`).
- **Context merge:** merge `globalContext` (defaults) with per-call `context`;
  per-call wins.
- **Init/shutdown:** init just stores context + sets READY; shutdown tears down
  the client and sets NOT_READY. Using the provider after shutdown MUST raise
  `PROVIDER_ERROR`.

---

## 15. Applicable variants

`getApplicableVariants(context, prefixFilter?, excludePrefixFilter?) -> List<String>`
returns the applicable experiment variant IDs (may be unordered; treat as a set).

### 15.1 Local provider
1. Ensure fresh (OnDemand) as in [§13](#13-local-provider-evaluation-algorithm).
2. If there is no cached experiment data → return `[]` (empty; **not** an error).
3. Merge context, extract targeting key + query data.
4. Call the core `getApplicableVariants(queryData, prefixFilter,
   excludePrefixFilter, targetingKey)`. A **missing/empty targeting key** MUST be
   passed to the core as an **empty string `""`** (let the core decide — it
   buckets an empty identifier as "matches no experiments"), consistent with the
   remote provider. Implementations MUST NOT short-circuit to `[]` purely because
   the targeting key is absent.
5. A core error MUST propagate (do not swallow).

### 15.2 Remote provider (REQUIRED)
Issue the "applicable variants" command with `identifier = targetingKey ?? ""`
(empty string when absent), the merged context, and the prefix filters; return
the variant IDs. **On API error the provider MUST propagate a `NETWORK_ERROR`**
— it MUST NOT swallow the error and return `[]`. An empty result set from a
*successful* call is `[]`.

---

## 16. Filtering (prefix / exclude_prefix)

- `prefixFilter` and `excludePrefixFilter` are lists of key prefixes, threaded
  through fetches, evaluation, resolve, and applicable-variants in the fixed
  parameter order ([§5](#5-data-source-interface)).
- **Empty means absent:** an empty (or null) filter list MUST be omitted from the
  request / treated as "no filter." Implementations MUST NOT send an empty list
  as a meaningful filter.
- In local evaluation, the filters are passed **both** to applicable-variant
  computation and to the core evaluate call.
- See [§6.4](#64-list-typed-query-parameters-implementation-note) for the SDK
  list-query serialization caveat.

---

## 17. Concurrency, memory & cancellation

- **Single in-flight refresh (SHOULD):** refreshes SHOULD be serialized (one at a
  time) so overlapping refreshes don't race on the cache.
- **Background-task lifetime (REQUIRED semantics):** a provider that is dropped
  **without** an explicit `shutdown()` MUST NOT be kept alive forever by its own
  background loop. The polling/watch loops MUST therefore reference the provider
  **weakly**, so a dropped provider becomes collectable and its loop stops on its
  own — on the next tick (polling) or when the watch channel closes (watch).
  Whatever the loop would otherwise pin MUST then be released:
  - **FFI clients (Python / JS / Java):** the native cache handle MUST be freed.
    This is done by the language's finalizer/registry (`__del__` /
    `FinalizationRegistry` / `Cleaner`), which can only run once the
    weak-referencing loop lets the provider (and thus the cache) be collected — so
    the weak reference is a prerequisite, not an optimization.
  - **Rust:** there is **no** native cache handle. Making the provider collectable
    lets `Drop` release the data sources and any OS file watcher via RAII. The
    loops hold a `Weak` and stop on upgrade failure / watch-channel close — **no
    cancellation token is required**; `close()` remains the explicit-teardown path.
- **Thread-safety:** all mutable cache/status state MUST be guarded (locks /
  atomics / single-threaded executor). `status()` SHOULD never block; degrading
  to `NOT_READY` under lock contention is acceptable.
- **Interruptible waits:** when blocking on an async result inside a
  timeout-bounded refresh, use an interruptible wait so a refresh past its
  timeout can actually be cancelled (do not use a non-interruptible join).

---

## 18. Options & validation

Provider construction takes: endpoint, auth, `org_id`, `workspace_id`, and
(local) primary/fallback sources + refresh strategy. `AuthMethod` is
`Token | Basic`. Options **SHOULD be validated at construction** (non-empty
endpoint, org, workspace, coherent auth), failing fast with a clear error, in
**every** language.

---

## 19. Conformance checklist

An implementation is conformant iff:

- [ ] Canonical contract names preserved per [§1.4](#14-canonical-contract-surface--the-named-contract-required) (`SuperpositionDataSource` + its 8 methods, `FetchResponse`/`ConfigData`/`ExperimentData`, `AllFeatureProvider`, `FeatureExperimentMeta`, `LocalResolutionProvider`, `SuperpositionAPIProvider`, `ErrorCode`, `RefreshStrategy`, `AuthMethod`); only casing/keyword adapts.
- [ ] `FetchResponse` is a real sum type; 304 ⇒ `NotModified`, distinct from empty data.
- [ ] `fetchedAt` = server `last_modified` (HTTP) / read-time (File); fed back as `if-modified-since`.
- [ ] File not-modified uses `mtime <= ifModifiedSince`; file watch observes the parent directory and reacts to create/rename, not only in-place modify.
- [ ] Type predicates exactly per [§4](#4-type-coercion-contract) (bool≠int, int rejects floats & bools, float widens int, object accepts array+object, no string coercion).
- [ ] Evaluation error mapping exactly per [§3.2](#32-openfeature-evaluation-error-mapping) (FLAG_NOT_FOUND / TYPE_MISMATCH / GENERAL, in that order).
- [ ] candidate = EXACT, matching = SUBSET.
- [ ] Refresh runs config+experiments concurrently; config error wins; any failure ⇒ STALE (from READY); recovery ⇒ READY; events emitted where supported.
- [ ] OnDemand TTL off the `checkedAt` clock, advanced on every check incl. 304.
- [ ] Init: config required; experiments required iff primary supports them; fallback init-only; exact error-message strings.
- [ ] Single-shot init guard; idempotent shutdown; post-shutdown ⇒ `PROVIDER_ERROR`.
- [ ] prefix/exclude_prefix threaded in the fixed order; empty ⇒ omitted; SDK list-query caveat handled on all clients.
- [ ] Applicable variants: local returns `[]` only when no cached experiments; missing key ⇒ `""`; core/API errors propagate.
- [ ] Options validated at construction; durations in ms with the default values in [§9](#9-refresh-strategies).
- [ ] Background loops don't pin/leak a dropped provider.

