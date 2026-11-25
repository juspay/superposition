# Tests and checks for Haskell Providers

run

```
make test-hs-provider
```

## Polling example

`polling-example` runs the OpenFeature provider with `Poll` refresh against the
staging deployment  and prints resolved values
while polling. Edit a config in Superposition during the watch window to see the
next poll pick it up.

```
cabal run --project-file=cabal.project.ci polling-example
```

Environment overrides: `SUPERPOSITION_ENDPOINT`, `SUPERPOSITION_ORG`,
`SUPERPOSITION_WORKSPACE`, `SUPERPOSITION_TOKEN`, `POLL_INTERVAL_SECONDS`,
`WATCH_SECONDS`.
