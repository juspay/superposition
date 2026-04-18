import { describe, it, expect } from "vitest";
import {
  contextMatchesScope,
  filterOverridesByScope,
  filterExperimentsByScope,
  mergeScopedContext,
  getLockedDimensions,
} from "../../src/utils/context-filter";
import type { ContextOverride, Experiment } from "../../src/types";

describe("contextMatchesScope", () => {
  it("matches when scoped context is empty", () => {
    expect(contextMatchesScope({ region: "us-east-1" }, {})).toBe(true);
  });

  it("matches when condition has same value", () => {
    expect(
      contextMatchesScope({ region: "us-east-1", env: "prod" }, { region: "us-east-1" }),
    ).toBe(true);
  });

  it("does not match when condition has different value", () => {
    expect(
      contextMatchesScope({ region: "eu-west-1" }, { region: "us-east-1" }),
    ).toBe(false);
  });

  it("matches when condition does not have the scoped key (broader scope)", () => {
    expect(contextMatchesScope({ env: "prod" }, { region: "us-east-1" })).toBe(true);
  });

  it("matches when condition value is array containing scoped value", () => {
    expect(
      contextMatchesScope(
        { region: ["us-east-1", "us-west-2"] },
        { region: "us-east-1" },
      ),
    ).toBe(true);
  });

  it("does not match when condition array does not contain scoped value", () => {
    expect(
      contextMatchesScope(
        { region: ["eu-west-1", "eu-central-1"] },
        { region: "us-east-1" },
      ),
    ).toBe(false);
  });

  it("matches with multiple scoped keys", () => {
    expect(
      contextMatchesScope(
        { region: "us-east-1", env: "prod", tier: "premium" },
        { region: "us-east-1", env: "prod" },
      ),
    ).toBe(true);
  });

  it("does not match when one key matches but another doesn't", () => {
    expect(
      contextMatchesScope(
        { region: "us-east-1", env: "staging" },
        { region: "us-east-1", env: "prod" },
      ),
    ).toBe(false);
  });
});

describe("filterOverridesByScope", () => {
  const overrides: ContextOverride[] = [
    {
      id: "1",
      value: { region: "us-east-1", env: "prod" },
      override_id: "o1",
      override_: { key: "val" },
      created_at: "",
      created_by: "",
      last_modified_at: "",
      last_modified_by: "",
      weight: "1",
      description: "",
      change_reason: "",
    },
    {
      id: "2",
      value: { region: "eu-west-1" },
      override_id: "o2",
      override_: { key: "val2" },
      created_at: "",
      created_by: "",
      last_modified_at: "",
      last_modified_by: "",
      weight: "1",
      description: "",
      change_reason: "",
    },
    {
      id: "3",
      value: { env: "prod" },
      override_id: "o3",
      override_: { key: "val3" },
      created_at: "",
      created_by: "",
      last_modified_at: "",
      last_modified_by: "",
      weight: "1",
      description: "",
      change_reason: "",
    },
  ];

  it("returns all when no scope", () => {
    expect(filterOverridesByScope(overrides)).toHaveLength(3);
    expect(filterOverridesByScope(overrides, {})).toHaveLength(3);
  });

  it("filters by region scope", () => {
    const result = filterOverridesByScope(overrides, { region: "us-east-1" });
    // Override 1: region matches → yes
    // Override 2: region is eu-west-1 ≠ us-east-1 → no
    // Override 3: no region key → yes (broader scope)
    expect(result.map((r) => r.id)).toEqual(["1", "3"]);
  });
});

describe("filterExperimentsByScope", () => {
  const experiments = [
    { id: "1", context: { region: "us-east-1" } },
    { id: "2", context: { region: "eu-west-1" } },
  ] as unknown as Experiment[];

  it("filters experiments by scope", () => {
    const result = filterExperimentsByScope(experiments, { region: "us-east-1" });
    expect(result.map((e) => e.id)).toEqual(["1"]);
  });
});

describe("mergeScopedContext", () => {
  it("merges scoped into user context", () => {
    const result = mergeScopedContext(
      { env: "prod" },
      { region: "us-east-1" },
    );
    expect(result).toEqual({ env: "prod", region: "us-east-1" });
  });

  it("scoped takes precedence", () => {
    const result = mergeScopedContext(
      { region: "eu-west-1", env: "prod" },
      { region: "us-east-1" },
    );
    expect(result).toEqual({ env: "prod", region: "us-east-1" });
  });

  it("returns user context when no scope", () => {
    const result = mergeScopedContext({ env: "prod" });
    expect(result).toEqual({ env: "prod" });
  });
});

describe("getLockedDimensions", () => {
  it("returns empty for no scope", () => {
    expect(getLockedDimensions()).toEqual([]);
    expect(getLockedDimensions(undefined)).toEqual([]);
  });

  it("returns scoped dimension keys", () => {
    expect(getLockedDimensions({ region: "us", env: "prod" })).toEqual([
      "region",
      "env",
    ]);
  });
});
