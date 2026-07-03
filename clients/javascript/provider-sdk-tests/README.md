# Superposition Provider SDK Tests

This package has two small Node apps:

- `npm run demo -w provider-sdk-tests` creates a small workspace and checks provider behavior.
- `npm run perf:resolve -w provider-sdk-tests` benchmarks hot provider resolution against an existing workspace.

## Resolve Performance Benchmark

The benchmark initializes the `SuperpositionProvider` once, then repeatedly calls either `provider.resolveAllConfigDetails` or a single OpenFeature flag lookup. This keeps config fetch/init time separate from the hot in-memory resolve path.

Run it after importing or creating the large override dataset you want to test:

```bash
cd clients/javascript
npm run build -w bindings -w sdk -w open-feature-provider

SUPERPOSITION_ENDPOINT=http://localhost:8081 \
SUPERPOSITION_TOKEN=12345678 \
SUPERPOSITION_ORG_ID=localorg \
SUPERPOSITION_WORKSPACE_ID=test \
RESOLVE_MODE=all \
RESOLVE_ITERATIONS=50000 \
RESOLVE_DIMENSIONS=merchant_id,country,platform \
RESOLVE_CARDINALITY=50000 \
npm run perf:resolve -w provider-sdk-tests
```

Useful options:

```bash
RESOLVE_MODE=all                         # all | number | string | boolean | object
RESOLVE_KEY=price                        # required for non-all modes
RESOLVE_ITERATIONS=100000
RESOLVE_WARMUP=5000
RESOLVE_CONCURRENCY=1
RESOLVE_CONTEXTS_FILE=./contexts.ndjson  # JSON array or newline-delimited JSON
RESOLVE_INCLUDE_TARGETING_KEY=true
```

For exact workload replay, put contexts in a file:

```json
{"merchant_id":"m1","country":"IN","platform":"ios"}
{"merchant_id":"m2","country":"US","platform":"android"}
```

Then run with `RESOLVE_CONTEXTS_FILE=./contexts.ndjson`.
