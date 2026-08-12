/**
 * The type-coercion contract: a flag resolves to its value only when the stored value matches the
 * requested type, otherwise TYPE_MISMATCH; a missing flag is FLAG_NOT_FOUND. Mirrors Python's
 * test_type_contract.py.
 *
 * Lives here (not next to the provider source) because the tests import the built `superposition-provider`
 * package — which pulls in the native binding — so they must run from the sdk-tests environment where
 * that package is installed, exactly like the integration harness (index.js). This one needs no live
 * server and no native call (it only constructs BaseResolutionProvider), but is kept alongside the
 * others for a single, uniform test location.
 *
 * Run standalone (from clients/javascript/provider-sdk-tests): node unit/test-type-contract.mjs
 */

import assert from "node:assert";
import { ErrorCode, StandardResolutionReasons } from "@openfeature/server-sdk";
import { BaseResolutionProvider } from "superposition-provider";

// A provider whose "resolved config" is a fixed map, so we test extraction in isolation.
class FakeProvider extends BaseResolutionProvider {
    metadata = { name: "FakeProvider" };
    constructor(config) {
        super();
        this._config = config;
    }
    async resolveAllFeaturesWithFilter() {
        return this._config;
    }
    async getApplicableVariants() {
        return [];
    }
}

const CONFIG = {
    aBool: true,
    aString: "hello",
    aNumber: 42,
    aFloat: 3.14,
    anObject: { k: "v" },
    anArray: [1, 2, 3],
};

const ctx = {};

async function test_correct_types_resolve() {
    const p = new FakeProvider(CONFIG);
    assert.strictEqual((await p.resolveBooleanEvaluation("aBool", false, ctx)).value, true);
    assert.strictEqual((await p.resolveStringEvaluation("aString", "", ctx)).value, "hello");
    assert.strictEqual((await p.resolveNumberEvaluation("aNumber", 0, ctx)).value, 42);
    assert.strictEqual((await p.resolveNumberEvaluation("aFloat", 0, ctx)).value, 3.14);
    assert.deepStrictEqual((await p.resolveObjectEvaluation("anObject", {}, ctx)).value, { k: "v" });
    // Arrays count as objects (allowed).
    assert.deepStrictEqual((await p.resolveObjectEvaluation("anArray", [], ctx)).value, [1, 2, 3]);
}

async function test_type_mismatch() {
    const p = new FakeProvider(CONFIG);
    // A string requested as a boolean is a mismatch, not a coercion.
    const asBool = await p.resolveBooleanEvaluation("aString", false, ctx);
    assert.strictEqual(asBool.value, false);
    assert.strictEqual(asBool.errorCode, ErrorCode.TYPE_MISMATCH);
    assert.strictEqual(asBool.reason, StandardResolutionReasons.ERROR);

    // A boolean must never be read as a number.
    const boolAsNum = await p.resolveNumberEvaluation("aBool", 0, ctx);
    assert.strictEqual(boolAsNum.value, 0);
    assert.strictEqual(boolAsNum.errorCode, ErrorCode.TYPE_MISMATCH);

    // A number requested as a string is a mismatch (no stringification).
    const numAsStr = await p.resolveStringEvaluation("aNumber", "d", ctx);
    assert.strictEqual(numAsStr.value, "d");
    assert.strictEqual(numAsStr.errorCode, ErrorCode.TYPE_MISMATCH);
}

async function test_flag_not_found() {
    const p = new FakeProvider(CONFIG);
    const missing = await p.resolveBooleanEvaluation("nope", true, ctx);
    assert.strictEqual(missing.value, true);
    assert.strictEqual(missing.errorCode, ErrorCode.FLAG_NOT_FOUND);
    assert.strictEqual(missing.reason, StandardResolutionReasons.ERROR);
}

async function test_general_error_on_throw() {
    const p = new FakeProvider(CONFIG);
    // Make resolution throw; the contract reports a GENERAL error and returns the default.
    p.resolveAllFeaturesWithFilter = async () => {
        throw new Error("boom");
    };
    const res = await p.resolveStringEvaluation("aString", "fallback", ctx);
    assert.strictEqual(res.value, "fallback");
    assert.strictEqual(res.errorCode, ErrorCode.GENERAL);
}

const CASES = [
    test_correct_types_resolve,
    test_type_mismatch,
    test_flag_not_found,
    test_general_error_on_throw,
];

for (const c of CASES) {
    try {
        await c();
        console.log(`ok  ${c.name}`);
    } catch (e) {
        console.error(`FAIL ${c.name}:`, e.message);
        process.exitCode = 1;
    }
}
if (!process.exitCode) {
    console.log(`\n${CASES.length} passed`);
}
