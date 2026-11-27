# JavaScript/TypeScript Config-File Binding Tests

This directory contains the TypeScript bindings for Superposition's native core, exposed
through the C FFI (Foreign Function Interface) layer, plus the test scripts that exercise
TOML and JSON config-file parsing.

> **Note**: JavaScript is not supported by uniffi, so these bindings use the
> `ffi_legacy` C ABI in `crates/superposition_core/src/ffi_legacy.rs` rather than the
> uniffi-generated interface used by the Kotlin and Python clients.

## Layout

| File                 | Purpose                                                     |
| -------------------- | ----------------------------------------------------------- |
| `native-resolver.ts` | The binding itself — `NativeResolver` class over the C ABI   |
| `test-toml.ts`       | TOML parsing tests                                          |
| `test-json.ts`       | JSON parsing tests                                          |
| `test-ffi.ts`        | Direct FFI smoke test against a fixed payload               |

## Prerequisites

1. **Build the native library:**

    ```bash
    cargo build --release -p superposition_core
    ```

2. **Make the library discoverable.** This step is easy to miss — see
   [Library discovery](#library-discovery) below. `cargo` emits
   `libsuperposition_core.dylib`, but the resolver searches for a target-triple-suffixed
   name, so on Apple silicon you need:

    ```bash
    cp target/release/libsuperposition_core.dylib \
       target/release/libsuperposition_core-aarch64-apple-darwin.dylib
    ```

3. **Install Node dependencies** (Node >= 18):

    ```bash
    cd clients/javascript/bindings
    npm install
    ```

## Running the Tests

The package is ESM TypeScript and the tests import compiled `.js` paths, so they must be
built first. There is no `npm test` script.

```bash
npm run build
node dist/test-toml.js
node dist/test-json.js
node dist/test-ffi.js
```

Run them from `dist/` rather than via a TypeScript runner: the relative paths inside the
tests (the native library lookup and the example-file lookup) are calibrated for the
compiled `dist/` layout and resolve incorrectly from the source directory.

## API Reference

Everything is exposed as methods on `NativeResolver`.

```typescript
import { NativeResolver } from "./native-resolver.js";

const resolver = new NativeResolver(); // optional: new NativeResolver(explicitLibPath)
```

The constructor never throws on a missing library — it warns and sets an unavailable flag.
Check before use:

```typescript
if (!resolver.isNativeAvailable()) {
    // every other method will throw
}
```

### `parseConfigFileWithFilters(content, format, dimensions?, filterPrefixes?, filterExcludePrefixes?)`

Parses a TOML or JSON configuration string into structured data.

**Parameters:**

- `content` (string): the configuration file contents
- `format` (string): `"toml"` or `"json"`
- `dimensions` (object, default `{}`): dimension data used to pre-filter contexts
- `filterPrefixes` (string[], default `[]`): only keys with these prefixes
- `filterExcludePrefixes` (string[], default `[]`): drop keys with these prefixes

**Returns:** an object of **parsed values** (not JSON strings):

- `default_configs` (object)
- `contexts` (array)
- `overrides` (object)
- `dimensions` (object)

**Example:**

```typescript
const toml = `
[default-configs]
per_km_rate = { "value" = 20.0, "schema" = { "type" = "number" } }

[dimensions]
city = { position = 1, schema = { "type" = "string", "enum" = ["Bangalore", "Delhi"] } }

[[overrides]]
_context_ = { city = "Bangalore" }
per_km_rate = 22.0
`;

const parsed = resolver.parseConfigFileWithFilters(toml, "toml");
console.log(parsed.default_configs); // { per_km_rate: { value: 20.0, schema: {...} } }
console.log(parsed.contexts); // array of context objects
```

### `resolveConfig(defaultConfigs, contexts, overrides, dimensions, queryData, mergeStrategy?, filterPrefixes?, filterExcludePrefixes?)`

Resolves a configuration against a set of query dimensions. `mergeStrategy` is
`"merge"` (default) or `"replace"`.

### `getApplicableVariants(experiments, experimentGroups, dimensions, userContext, identifier, filterPrefixes?, filterExcludePrefixes?)`

Returns the variant IDs (`string[]`) applicable to the given context and identifier.

### `createProviderCache()`

Returns a handle wrapping the native `ProviderCache`, which holds parsed config in native
memory so repeated evaluations skip re-parsing:

```typescript
const cache = resolver.createProviderCache();
cache.initConfig(defaultConfigs, contexts, overrides, dimensions);
cache.initExperiments(experiments, experimentGroups);
const resolved = cache.evalConfig(queryData, "merge", prefixes, excludePrefixes, targetingKey);
cache.free(); // required — the cache is not garbage collected
```

`free()` must be called explicitly. The handle owns native memory that Node's GC cannot
reclaim.

## Test Coverage

### `test-toml.ts`

1. **Parse TOML Configuration** — parses an inline ride-sharing pricing example and prints
   the default config, contexts, overrides, and dimensions.
2. **Parse External TOML File** — parses
   `examples/superposition_config_file_examples/example.toml`. Reported as *skipped*
   (not failed) when the file is absent.
3. **Error Handling** — three malformed inputs: invalid TOML syntax, a missing required
   section, and a dimension missing its `position`. Informational; does not affect the
   pass/fail tally.

The summary counts only non-skipped tests, so a full local run prints `2/2` and a run
without the example file prints `1/1`.

```text
======================================================================
  TEST SUMMARY
======================================================================
  ✓ Parse TOML
  ✓ External File

  Total: 2/2 tests passed
======================================================================
```

The process exits `0` when every non-skipped test passes, `1` otherwise.

## Merge Strategies

- `"merge"` (default): merges override values into the default configuration
- `"replace"`: replaces the value outright with the override

## Error Handling

Methods throw standard `Error` objects, but the message shape differs by call:

| Source                            | Message shape                       |
| --------------------------------- | ----------------------------------- |
| `parseConfigFileWithFilters`      | `TOML parsing failed: <native msg>` |
| `ProviderCache` methods           | `ffi: <native msg>`                 |
| Argument validation               | `<param> parameter is required`     |
| Library unavailable               | `Native resolver is not available.` |

```typescript
try {
    resolver.parseConfigFileWithFilters(invalidToml, "toml");
} catch (error) {
    console.error("Parsing failed:", error.message);
}
```

> The `TOML parsing failed:` prefix is hard-coded and appears even when `format` is
> `"json"`. Match on the native message rather than the prefix.

## Library discovery

`getDefaultLibPath()` builds a target-triple-suffixed filename and searches for it:

| Platform            | Filename                                                 |
| ------------------- | -------------------------------------------------------- |
| macOS (arm64)       | `libsuperposition_core-aarch64-apple-darwin.dylib`        |
| macOS (x64)         | `libsuperposition_core-x86_64-apple-darwin.dylib`         |
| Windows (x64)       | `libsuperposition_core-x86_64-pc-windows-msvc.dll`        |
| Linux (fallback)    | `libsuperposition_core-x86_64-unknown-linux-gnu.so`       |

Search order, relative to the compiled module in `dist/`:

1. `clients/javascript/bindings/<filename>`
2. `clients/javascript/bindings/dist/native-lib/<filename>`
3. `clients/javascript/bindings/native-lib/<filename>`
4. `clients/javascript/bindings/native-lib/<platform>-<arch>/<filename>`
5. `target/release/<filename>` (repository root)
6. the bare filename, left to the system loader

> **Gotcha:** `cargo build --release` produces `libsuperposition_core.dylib`, which matches
> *none* of these — the suffixed name is what CI artifacts use. A comment in
> `native-resolver.ts` promises a "simple library name" fallback, but the `simpleLibName`
> variable it computes is never read, so that fallback does not exist. Either copy the file
> to a suffixed name (see Prerequisites) or pass an explicit path to the constructor:
> `new NativeResolver("/abs/path/to/libsuperposition_core.dylib")`.

## Using in Your Project

Prefer the [OpenFeature provider](https://www.npmjs.com/package/superposition-provider),
which wraps this package. To depend on the bindings directly:

```bash
npm install superposition-bindings
```

```typescript
import { NativeResolver } from "superposition-bindings";

const resolver = new NativeResolver();
const config = resolver.resolveConfig(
    defaultConfigs,
    contexts,
    overrides,
    dimensions,
    { city: "Bangalore", vehicle_type: "cab" },
    "merge",
);
```

## Memory Management

- C strings returned across the FFI are copied into JavaScript and freed with
  `core_free_string` before the call returns.
- Error buffers (2048 bytes) are allocated per call and released with the surrounding
  scope.
- The one exception is `createProviderCache()`, whose handle must be released with
  `free()`.

## TOML Structure

```toml
[default-configs]
key1 = { "value" = <value>, "schema" = <json-schema> }

[dimensions]
dim1 = { position = 1, schema = <json-schema> }

[[overrides]]
_context_ = { dim1 = "value1" }
key1 = <override-value>

[[overrides]]
_context_ = { dim1 = "value1", dim2 = "value2" }
key1 = <override-value>
```

Note the section is `[default-configs]` (plural), and every dimension needs a `position`.
See `test-toml.ts` for a complete ride-sharing pricing example.

## Technical Details

### C FFI Signatures

`native-resolver.ts` binds these symbols via [koffi](https://koffi.dev):

```c
char* core_get_resolved_config(const char*, const char*, const char*, const char*,
                               const char*, const char*, const char*, const char*,
                               const char*, const char*);
char* core_get_applicable_variants(const char*, const char*, const char*, const char*,
                                   const char*, const char*, const char*, char*);
char* core_parse_config_file_with_filters(const char*, const char*, const char*,
                                          const char*, const char*, char*);

void* core_provider_cache_new();
void  core_provider_cache_free(void*);
void  core_provider_cache_init_config(void*, const char*, const char*, const char*,
                                      const char*, char*);
void  core_provider_cache_init_experiments(void*, const char*, const char*, char*);
char* core_provider_cache_eval_config(void*, const char*, const char*, const char*,
                                      const char*, const char*, char*);

int   core_test_connection();
void  core_free_string(char*);
```

Every fallible entry point takes a trailing `char* error_buffer`; the binding allocates
2048 bytes, and a non-empty buffer after the call is raised as an `Error`.

## Troubleshooting

### "Native resolver library not available"

The constructor warns rather than throws, so this surfaces as every subsequent call
failing with *"Native resolver is not available."* Check:

1. The library is built: `cargo build --release -p superposition_core`
2. It is named and placed per [Library discovery](#library-discovery) — this is the most
   common cause
3. You are running the compiled `dist/` output, not the `.ts` sources

### External file test is skipped

`examples/superposition_config_file_examples/example.toml` was not found relative to the
repository root. Expected when running from an installed package rather than a checkout.

## Development

1. **native-resolver.ts** — FFI bindings and the `NativeResolver` class
2. **test-toml.ts / test-json.ts / test-ffi.ts** — test scripts
3. **package.json / tsconfig.json** — build configuration (ESM, output to `dist/`)

After changing the Rust FFI layer, rebuild both sides:

```bash
cargo build --release -p superposition_core
npm run build && node dist/test-toml.js
```
