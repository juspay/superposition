# Default-config symlinks

**Status:** approved design, not yet implemented
**Date:** 2026-10-06

## Intent

Let a default-config key be a symlink to another default-config key, so that both
names always resolve to the same value. A config key is treated as a file: the
link's *content* is the target's key name, and a flag in its schema marks it as a
link — the same shape a POSIX symlink has.

Two use cases drive this, both confirmed:

1. **Key rename / migration.** A key is renamed but deployed clients still ask for
   the old name. The old name becomes a symlink to the new one, so both resolve
   identically until the old name can be retired.
2. **One canonical value, many names.** Several keys must always carry the same
   value. They symlink to a canonical key so a change lands in one place.

Explicitly **not** driving this design: cross-workspace references, and symlinking
a whole key prefix / subtree.

## Agreed semantics

| Question | Decision |
|---|---|
| Override or experiment variant targeting a link | Rewritten to the target before storage, so both names are always equal — under defaults *and* under every context |
| Value update on a link | Redirects to the target |
| Deleting a key that links point at | Blocked, with a 400 naming the dependents |
| Link chains | Impossible: a link's target is resolved to a concrete key at write time |
| Links in override / variant key pickers | Offered like any other key; a link is a key everywhere |

The invariant the whole feature exists to provide, and the first test to write:

```
for any query_data:  eval(config)[link] == eval(config)[target]
```

## Representation

A link is an ordinary `default_configs` row. No DDL, no new table, no new column —
which matters because this repo has no automated path for rolling DDL out to
existing per-workspace schemas: Diesel migrations only touch `public.*`, and
`workspace_template.sql` is applied once, at workspace creation
(`superposition/src/workspace/handlers.rs:65`). A representation inside the
existing `schema` column lights up on every existing workspace the moment the
server ships.

Stored row:

```json
{
  "key": "payments.retry_count",
  "value": "payments.retry.count",
  "schema": {
    "type": "string",
    "pattern": "^[a-zA-Z0-9-_]([a-zA-Z0-9-_.]{0,254}[a-zA-Z0-9-_])?$",
    "x-superposition-symlink": true
  },
  "description": "Renamed to payments.retry.count; kept for pre-4.2 mobile clients",
  "change_reason": "key rename"
}
```

- `value` holds the target key name. The row is therefore **self-validating**: the
  schema genuinely describes the value, so the existing validation gate in
  `create_handler` (`default_config/handlers.rs:115-131`) does real work on link
  rows instead of passing vacuously.
- `pattern` is `ALPHANUMERIC_WITH_DOT` verbatim (`superposition_types/src/lib.rs:170`),
  the same regex `DefaultConfigKey` validates against, so the pointer is checked by
  JSON Schema through the code path that already runs.
- `x-superposition-symlink` is the boolean `true` — **not** a repeat of the target
  name. A pointer with two homes can disagree with itself.
- A write may send the short form `{"x-superposition-symlink": true}`; the server
  normalizes it to the canonical schema above, so the regex is never duplicated
  into the frontend.
- A schema carrying the flag must carry no other keywords beyond `type` and
  `pattern`, so "is this a link?" stays a yes/no question rather than a parse.
- `value_validation_function_name` and `value_compute_function_name` are **rejected**
  on a link write, not ignored. Accepting them would tell an operator their values
  are validated when nothing of the sort is happening. The target's functions govern
  in practice, since every write to a link redirects to the target.

### Why not `$ref`

- *Semantic:* `$ref` asserts "my schema **is** that schema" — schema reuse. A symlink
  asserts "my **value** lives there". Spending `$ref` on the second forecloses the
  first, which this codebase is plausibly headed toward after 86ecf23a widened the
  accepted schema shapes.
- *Mechanical:* **this argument was wrong as first written, and the correction matters.**
  The spec originally claimed a `superposition://` ref would fail to compile, since
  `try_into_jsonschema` is a bare `JSONSchema::compile` with no resolver
  (`superposition_core/src/validations.rs:50-55`). A test written to confirm that
  showed the opposite: `jsonschema ~0.17` compiles a schema with an unresolvable
  external `$ref` without complaint. Both facts are now pinned by tests in that file —
  `an_external_ref_cannot_resolve_here` and
  `an_unknown_keyword_compiles_and_validates_anything`.

  What survives is a weaker but still real point, and the semantic argument above does
  the actual work. A `$ref` form would store a schema that compiles while pointing at
  something nothing can resolve, so whatever it validates is unspecified and would have
  to be characterised before being relied on; the vendor keyword's behaviour is known
  and verified — Draft-7 ignores keywords it does not recognise. Making a `$ref`
  genuinely resolve would still mean threading a DB-reading resolver into
  `superposition_core`, a crate that is otherwise pure and shared with clients.

### Integrity, enforced in the application

Because the pointer lives in JSON rather than a foreign key, three rules replace what
`ON DELETE RESTRICT` would have given:

1. **Pre-delete scan.** Deleting a key first looks for dependents and returns a 400
   listing them — the same shape as the existing context-usage check at
   `default_config/handlers.rs:512`:
   ```sql
   WHERE schema->>'x-superposition-symlink' = 'true'
     AND value #>> '{}' = $1
   ```
   The comparison is on text rather than a `::boolean` cast: a cast raises on a
   hand-edited non-boolean marker, which would fail the whole query and take config
   assembly down with it — the opposite of rule 3 below.
2. **Flatten at write.** If the requested target is itself a link, store the concrete
   key it resolves to. Depth stays 1 and cycles are impossible by construction.
   A serial rename (`old -> new`, then `new -> newer`) cannot leave a chain, because
   `new` cannot be deleted while `old` depends on it, so the operator must repoint.
3. **Defensive expansion.** An unresolvable or self-referential pointer logs ERROR and
   the key is omitted from the payload, so a hand-edited row cannot break config
   assembly for the whole workspace.

## Read path

Expansion turns a link into a real entry in the served payload. **Clients do not
change at all** — `cac_client`, `superposition_provider`, the generated SDKs, the
uniffi bindings and the OpenFeature providers all keep seeing a flat key/value map,
which is the point: in a rename, the old clients are by definition the ones that
cannot be redeployed.

```json
{
  "default_configs": {
    "payments.retry.count": 3,
    "payments.retry_count": 3
  },
  "overrides": {
    "7b1f…": {
      "payments.retry.count": 5,
      "payments.retry_count": 5
    }
  }
}
```

The entry inside the **override map** is the half that is easy to miss. Expand only
`default_configs` and the old name silently freezes at its default while the new one
moves under a matching context.

### Where expansion runs, and where it must not

| Site | Role | Expand? |
|---|---|---|
| `add_config_version` → `config_versions.config` (`helpers.rs:219`) | the snapshot every write produces | yes |
| `put_config_in_redis` → `{schema}::{version}` (`helpers.rs:256`) | the cache clients read through | yes |
| `generate_config_from_version` fallback (`config/helpers.rs:139,144`) | no snapshot, or decode failure | yes |
| `reduce_handler` (`config/handlers.rs:465,485`) | rewrites contexts and, with `x-approve`, writes them back | **never** |

`reduce_config_key` recomputes override ids by hashing override contents
(`config/handlers.rs:412`). Given an expanded config, every override carrying an
aliased key would hash over alias-inflated contents, producing ids that disagree
with what the write path produces for the same semantic override — and then persist
them. This is why the distinction is a type, not a convention:

```rust
pub struct RawConfig(Config);

impl RawConfig {
    /// Adds the aliases. The result is what may be persisted or served.
    pub fn expand(self, links: &[SymlinkRow]) -> Config { .. }

    /// The config without aliases. Only for maintenance paths that write contexts
    /// back; never serve or snapshot this.
    pub fn into_unexpanded(self) -> Config { .. }
}

pub fn generate_cac(..) -> RawConfig    // today's query + one filter
```

One newtype carries the distinction, because the asymmetry is in the naming: every
serving path reaches config through `generate_config_from_version`, which expands
internally, so no serving path can forget. `into_unexpanded` is the single named
escape hatch, and `grep` for it should only ever find `reduce_handler`. A second
`ServedConfig` type would churn roughly eight handler signatures without adding a
guarantee.

`RawConfig` lives server-side, in `context_aware_config`. `superposition_types::Config`
itself is **unchanged**, so no client, SDK or uniffi binding sees a new type — only the
three or four server call sites do.

`generate_cac` gains one predicate — skip rows whose schema carries the flag — so
`RawConfig` is exactly today's config and **`reduce` needs no changes**. Links come
back from a second small query and expansion re-adds them:

```rust
for (link, target) in links {
    match default_configs.get(target) {
        Some(v) => { default_configs.insert(link.clone(), v.clone()); }
        None => { log::error!("symlink {link} -> {target}: target missing, key omitted"); continue }
    }
    for overrides in overrides.values_mut() {
        if let Some(v) = overrides.get(target) { overrides.insert(link.clone(), v.clone()); }
    }
}
```

**Expansion is write-time, not read-time.** Snapshots and the Redis entry are written
once per config change, so the hot path — snapshot or cache straight to the client —
pays nothing, and is byte-identical to today for a workspace with no links.

`generate_detailed_cac` (`helpers.rs:161-203`) needs the same treatment plus one
addition: a link's entry takes the target's value **and** schema, which is what makes
the TOML and JSON dumps, `resolve_detailed` and `explain` show a real type.

### Two consequences on the record

- **Prefix filtering orders itself correctly.** `apply_prefix_filter_to_config`
  (`config/helpers.rs:46`) runs at serve time against an already-expanded snapshot.
  A filter admitting the old prefix but excluding the new one still resolves the old
  key, because its value was copied during expansion — so old clients keep working
  through a rename even with a prefix filter pinned to the old namespace. And since
  both names exist by then, `apply_overrides_on_default_config` logs nothing new: no
  regression on the ERROR-noise cleanup in #1161.
- **Override ids intentionally do not change under expansion.** Expansion mutates
  override contents while leaving the content-hash id alone. No client re-derives an
  id from a served payload; `reduce` does, and the type split keeps it away from
  expanded data.

Experiments are unaffected: they fetch `/config` over HTTP
(`experiments/helpers.rs:420-437`) and so receive served config, which is correct
because experiment writes are normalized to targets before anything is stored.

## Write path

### Normalize, then authorize

Authorization is keyed by config key name — `create_authorized` authorizes
`override_map.keys()` (`context/handlers.rs:84-91`), and the default-config handlers
authorize on the key itself (`default_config/handlers.rs:81`). The order is therefore
mandatory: **normalize first, then authorize on the target.** The other order makes a
symlink an authority-widening device — grant someone `old.key`, they write to it, and
the change lands on `new.key`, which policy never let them touch.

Consequence: write-grants held only on the old name begin returning 403, so a rename
needs its grants repointed — one line in the runbook. Config *reads* are workspace-
and prefix-scoped rather than per-key, so readers are unaffected, which is where the
compatibility value lies.

**Creating** a link authorizes both the new key and its target. Without the second
check, a principal could create `allowed.alias -> restricted.key` and surface the
target's value under a name that a prefix-scoped reader is permitted to see. The
redirect on a later PATCH is already authorized against the target, so this closes
the remaining direction.

### Inventory

Every site that names a config key in a write, all funnelling through one
normalization helper applied before hashing and before authorizing:

| Crate | Endpoints |
|---|---|
| `default_config` | create, update, delete |
| `context` | create, update, move, bulk-operations, validate |
| `experiments` | create, update (variant overrides), conclude (applies the winning variant into CAC) |

Normalization must run **before `hash(&ctx_override)`** (`context/operations.rs:124-135`).
Override ids are content hashes, so normalizing afterwards yields two ids for one
semantic override. `validate_override_with_default_configs` itself needs no change —
by the time it runs, every key is a target.

For experiments, both the CAC context *and* the stored `variants` / `override_keys`
must be normalized, or the two diverge.

### PATCH discriminator

One rule covers create and update alike: *a write carrying the symlink flag addresses
the link; a write without it addresses the value, and therefore redirects.*

| Request on a link | Effect |
|---|---|
| `{value: 5}`, or any `schema` / function name without the flag | redirects to the target |
| `{description: "…", change_reason: "…"}` only | updates the link's own row |
| `{value: "other.key", schema: {"x-superposition-symlink": true}}` | repoints the link |

## API changes

`GetDefaultConfig`, `ListDefaultConfigs`, `DefaultConfigInfo` in `DetailedConfig`, and
`fetch_default_config_metadata` (`config/helpers.rs:301`) all **resolve** a link before
responding. The UI therefore renders a real type without knowing symlinks exist, and
the existing schema-driven form (`default_config_form.rs`, including the anyOf /
type-less handling from 86ecf23a) is untouched.

Resolving rather than returning the raw row also preserves the contract:
`DefaultConfigMixin` marks `$value` and `$schema` `@required`
(`smithy/models/default-config.smithy:33-47`) and `DefaultConfigResponse` is built from
that mixin. Because a link's response carries the target's value and schema, both stay
required and non-null — no generated SDK gains an `Option` where it had a concrete type.

A link's response splits cleanly:

| From the **target** | The link's **own** |
|---|---|
| `value` | `key` |
| `schema` | `description` |
| `value_validation_function_name` | `change_reason` |
| `value_compute_function_name` | `created_at/by`, `last_modified_at/by`, audit history |

The Smithy diff is one optional property:

```smithy
resource DefaultConfig {
    properties: {
        ...
        symlink_to: String
    }
}
```

listed on `DefaultConfigResponse` only — which already adds members beyond the mixin
(`$created_at` and friends) — so it is **response-only** and absent on ordinary keys.
`CreateDefaultConfig` and `UpdateDefaultConfig` inputs are unchanged: a symlink create
sends `value` plus the flagged `schema`, satisfying the existing `@required` on both.
Then `make smithy-clients` regenerates the committed SDKs (Rust under `crates/`, plus
Java, Python, Haskell and JavaScript under `clients/`).

Links are ordinary rows, so the existing `default_configs_audit` trigger
(`workspace_template.sql:131`) gives them full audit history, and `ListDefaultConfigs`
already returns them.

## UI

Additive only:

- A badge in the default-config list page showing `→ target`.
- A symlink mode on the create form: key, target picker, description, change reason,
  with the value and schema inputs hidden.
- Links appear in the override and variant key pickers like any other key, rendered
  with the same `→ target` badge so the write-through is visible before commit rather
  than as a surprise on reload.
- The override form dedupes when a picked link and its target would both land in one
  override map, so the user is not left watching two rows collapse into one on save.

Two hazards specific to this frontend, to be designed in rather than rediscovered: a
new dropdown subtree is the shape that caused the workspaces-page hydration panic
(#1148), so the target picker is gated behind `client_side_ready`; and browser
verification must use `--release` wasm against the actually served site directory,
since `--dev` hydrate hard-aborts on disposed signals in dropdowns and a stale site
dir silently tests old wasm.

## Testing

Leading with the invariant, which is the spec in executable form:

```
for any query_data:  eval(config)[link] == eval(config)[target]
```

covering defaults only, under a matching context, under both merge strategies, with
two links to one target, and with a prefix filter admitting only the old namespace.

Then:

- `expand_symlinks` with a missing target: key omitted, ERROR logged, assembly survives.
- `generate_cac` omits link rows; `RawConfig` is unchanged for a workspace with links.
- Creating a link whose target is a link flattens to the concrete key.
- Function names on a link write are rejected.
- A schema carrying the flag plus unrelated keywords is rejected.
- The three PATCH cases in the discriminator table.
- Deleting a target with dependents returns 400 naming them; deleting a link succeeds.
- An override written against `old.key` produces a stored override byte-identical, and
  with the same override id, to one written against `new.key` directly.
- An experiment variant authored on `old.key` leaves both the experiment row and the
  CAC context naming `new.key`.
- Authorization: a principal granted only `old.key` is denied a write that redirects
  to `new.key`.
- The zero-client-change claim is verified, not asserted: the Haskell and JS
  OpenFeature provider integration tests now in CI (04378318) see both keys appear
  with no client modification.

## Rollout

No DDL and no migration. The feature is live for every existing workspace as soon as
the server ships, because the representation uses columns that already exist. The only
build step is `make smithy-clients` for the `symlink_to` response field.

## Out of scope

Recorded as decisions, not omissions:

- **Cross-workspace or cross-org links.** Not needed by either driving use case.
- **Prefix / subtree links.** Likewise.
- **Client-side dereference** — shipping a `symlinks` map in the payload and resolving
  at the edge. Smaller payload, but it needs an implementation in every client and
  language binding, and any client lacking it drops the old key entirely, which breaks
  the migration case precisely when it is needed. Revisit only as a payload-size
  optimization behind a client capability.
