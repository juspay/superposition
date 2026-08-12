/**
 * OnDemand TTL is driven by a local "checked-at" clock that advances on every successful check —
 * including a 304 Not Modified — not by the server's last-modified. So a config that stays unchanged
 * does NOT trigger a fetch on every evaluation. Mirrors Python's test_ondemand_ttl.py.
 *
 * Needs the native binding (the provider builds an FFI cache and evaluates through it), but no live
 * server — the data source is a fake.
 *
 * Run standalone (from clients/javascript/provider-sdk-tests): node unit/test-ondemand-ttl.mjs
 */

import assert from "node:assert";
import {
    LocalResolutionProvider,
    FetchResponse,
    RefreshStrategy,
} from "superposition-provider";

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

const CONFIG = {
    default_configs: { flag: "value" },
    contexts: [],
    overrides: {},
    dimensions: {},
};

/** Counts config fetches; the first returns data, every later conditional fetch returns 304. */
class CountingSource {
    constructor() {
        this.configFetches = 0;
    }
    async fetchConfig(ifModifiedSince) {
        this.configFetches += 1;
        if (ifModifiedSince === undefined) {
            return FetchResponse.data({ data: CONFIG, fetchedAt: new Date() });
        }
        // Unchanged since last time → 304. The provider must still advance its TTL clock.
        return FetchResponse.notModified();
    }
    async fetchFilteredConfig(_c, _p, ifModifiedSince) {
        return this.fetchConfig(ifModifiedSince);
    }
    async fetchActiveExperiments() {
        return FetchResponse.notModified();
    }
    async fetchCandidateActiveExperiments() {
        return FetchResponse.notModified();
    }
    async fetchMatchingActiveExperiments() {
        return FetchResponse.notModified();
    }
    supportsExperiments() {
        return false;
    }
    watch() {
        return null;
    }
    async close() {}
}

async function test_stable_config_does_not_refetch_every_eval() {
    const source = new CountingSource();
    const ttlMs = 60;
    const provider = new LocalResolutionProvider(
        source,
        undefined,
        RefreshStrategy.onDemand(ttlMs)
    );

    await provider.initialize({});
    assert.strictEqual(source.configFetches, 1, "init fetches once");

    // Within the TTL window: no refetch.
    await provider.resolveAllFeatures({});
    assert.strictEqual(source.configFetches, 1, "eval within TTL must not refetch");

    // Past the TTL: one refetch, which comes back 304.
    await sleep(ttlMs + 40);
    await provider.resolveAllFeatures({});
    assert.strictEqual(source.configFetches, 2, "eval past TTL refetches once");

    // The 304 advanced the checked-at clock, so an immediate eval must NOT refetch again.
    // (The bug this guards against drove the TTL off the server's last-modified, so a 304 left the
    // clock untouched and every subsequent eval hammered the backend.)
    await provider.resolveAllFeatures({});
    assert.strictEqual(
        source.configFetches,
        2,
        "a 304 must advance the TTL clock; the next eval must not refetch"
    );

    await provider.shutdown();
}

const CASES = [test_stable_config_does_not_refetch_every_eval];

for (const c of CASES) {
    try {
        await c();
        console.log(`ok  ${c.name}`);
    } catch (e) {
        console.error(`FAIL ${c.name}:`, e && e.stack ? e.stack : e);
        process.exitCode = 1;
    }
}
if (!process.exitCode) {
    console.log(`\n${CASES.length} passed`);
}
