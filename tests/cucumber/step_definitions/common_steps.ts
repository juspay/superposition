import { Then } from "@cucumber/cucumber";
import { SuperpositionWorld } from "../support/world.ts";
import * as assert from "node:assert";

// ── Generic outcome assertions ──────────────────────────────────────

Then("the operation should succeed", function (this: SuperpositionWorld) {
  assert.strictEqual(this.lastError, undefined, `Operation failed unexpectedly: ${this.lastError?.message}`);
  assert.ok(this.lastResponse !== undefined, "Expected a successful response but got none");
});

Then("the operation should fail", function (this: SuperpositionWorld) {
  assert.ok(this.lastError !== undefined, "Expected an error but operation succeeded");
});

Then(
  "the operation should fail with error matching {string}",
  function (this: SuperpositionWorld, errorPattern: string) {
    assert.ok(this.lastError !== undefined, "Expected an error but operation succeeded");
    // The SDK may throw a SyntaxError when the server returns non-JSON error responses.
    // In that case, the raw server response is available on $response.body.
    const rawBody = typeof this.lastError?.$response?.body === "string"
      ? this.lastError.$response.body
      : "";
    const message = rawBody || this.lastError?.message || String(this.lastError);
    assert.ok(
      message.includes(errorPattern),
      `Error "${message}" does not contain "${errorPattern}"`
    );
  }
);

// ── Generic response property assertions ────────────────────────────

Then(
  "the response should have an {string} property",
  function (this: SuperpositionWorld, prop: string) {
    assert.ok(this.lastResponse, "No response available");
    assert.ok(
      this.lastResponse[prop] !== undefined,
      `Response missing property "${prop}"`
    );
  }
);

Then(
  "the response should have a {string} property",
  function (this: SuperpositionWorld, prop: string) {
    assert.ok(this.lastResponse, "No response available");
    assert.ok(
      this.lastResponse[prop] !== undefined,
      `Response missing property "${prop}"`
    );
  }
);

Then("the response should contain a list", function (this: SuperpositionWorld) {
  assert.ok(this.lastResponse, "No response available");
  const data = this.lastResponse.data ?? this.lastResponse;
  assert.ok(Array.isArray(data), "Response is not a list");
});

Then(
  "the response should contain a list with at least {int} item(s)",
  function (this: SuperpositionWorld, count: number) {
    assert.ok(this.lastResponse, "No response available");
    const data = this.lastResponse.data ?? this.lastResponse;
    assert.ok(Array.isArray(data), "Response is not a list");
    assert.ok(data.length >= count, `Expected at least ${count} items, got ${data.length}`);
  }
);

Then(
  "the response should contain a list with at most {int} item(s)",
  function (this: SuperpositionWorld, count: number) {
    assert.ok(this.lastResponse, "No response available");
    const data = this.lastResponse.data ?? this.lastResponse;
    assert.ok(Array.isArray(data), "Response is not a list");
    assert.ok(data.length <= count, `Expected at most ${count} items, got ${data.length}`);
  }
);

Then("the response should have a version", function (this: SuperpositionWorld) {
  assert.ok(this.lastResponse, "No response available");
  assert.ok(this.lastResponse.version !== undefined, "Response missing version");
});

Then(
  "the response description should be {string}",
  function (this: SuperpositionWorld, expected: string) {
    assert.ok(this.lastResponse, "No response available");
    assert.strictEqual(this.lastResponse.description, expected);
  }
);
