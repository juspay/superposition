import { OpenFeature } from "@openfeature/server-sdk";
import { SuperpositionProvider } from "superposition-provider";
import { readFileSync } from "node:fs";
import { performance } from "node:perf_hooks";

const DEFAULT_ENDPOINT = "http://localhost:8080";
const DEFAULT_TOKEN = "12345678";

function parseInteger(name, defaultValue, { min = 0 } = {}) {
  const value = process.env[name];
  if (value === undefined || value === "") {
    return defaultValue;
  }

  const parsed = Number.parseInt(value, 10);
  if (!Number.isFinite(parsed) || parsed < min) {
    throw new Error(`${name} must be an integer >= ${min}`);
  }

  return parsed;
}

function parseBoolean(name, defaultValue) {
  const value = process.env[name];
  if (value === undefined || value === "") {
    return defaultValue;
  }

  return ["1", "true", "yes", "y"].includes(value.toLowerCase());
}

function parseDefaultValue(mode) {
  const raw = process.env.RESOLVE_DEFAULT;
  if (raw === undefined) {
    switch (mode) {
      case "boolean":
        return false;
      case "number":
        return 0;
      case "object":
        return {};
      case "string":
      default:
        return "";
    }
  }

  switch (mode) {
    case "boolean":
      return parseBoolean("RESOLVE_DEFAULT", false);
    case "number": {
      const parsed = Number(raw);
      if (!Number.isFinite(parsed)) {
        throw new Error("RESOLVE_DEFAULT must be a number for RESOLVE_MODE=number");
      }
      return parsed;
    }
    case "object":
      return JSON.parse(raw);
    case "string":
    default:
      return raw;
  }
}

function printHelp() {
  console.log(`
Superposition provider resolve benchmark

Required:
  SUPERPOSITION_ORG_ID             Organisation id
  SUPERPOSITION_WORKSPACE_ID       Workspace id

Common options:
  SUPERPOSITION_ENDPOINT           Default: ${DEFAULT_ENDPOINT}
  SUPERPOSITION_TOKEN              Default: ${DEFAULT_TOKEN}
  RESOLVE_MODE                     all | number | string | boolean | object. Default: all
  RESOLVE_KEY                      Config key for non-all modes
  RESOLVE_ITERATIONS               Timed resolve calls. Default: 10000
  RESOLVE_WARMUP                   Warmup resolve calls. Default: min(1000, iterations)
  RESOLVE_CONCURRENCY              Parallel async workers. Default: 1
  RESOLVE_DIMENSIONS               Comma-separated context dimensions. Default: city,tenant,user_segment
  RESOLVE_CARDINALITY              Generated values per dimension. Default: 1000
  RESOLVE_CONTEXTS_FILE            JSON array or newline-delimited JSON contexts
  RESOLVE_CONTEXT_POOL_SIZE        Generated context pool size. Default: min(iterations, 50000)
  RESOLVE_LATENCY_SAMPLES          Max sampled latencies. Default: 50000
  RESOLVE_INCLUDE_TARGETING_KEY    Add targetingKey to generated contexts. Default: false
  RESOLVE_TARGETING_KEY_PREFIX     Default: bench-user

Example:
  SUPERPOSITION_ENDPOINT=http://localhost:8081 \\
  SUPERPOSITION_ORG_ID=localorg \\
  SUPERPOSITION_WORKSPACE_ID=test \\
  RESOLVE_MODE=all \\
  RESOLVE_ITERATIONS=50000 \\
  RESOLVE_DIMENSIONS=merchant_id,country,platform \\
  npm run perf:resolve -w provider-sdk-tests
`);
}

function requireEnv(name, fallbackName) {
  const value = process.env[name] || (fallbackName ? process.env[fallbackName] : undefined);
  if (!value) {
    throw new Error(`${name} is required`);
  }
  return value;
}

function percentile(sortedValues, percentileValue) {
  if (sortedValues.length === 0) {
    return 0;
  }

  const index = Math.min(
    sortedValues.length - 1,
    Math.ceil((percentileValue / 100) * sortedValues.length) - 1,
  );
  return sortedValues[index];
}

function formatMs(value) {
  return Number(value.toFixed(4));
}

function summarizeLatencies(latencies) {
  const sorted = latencies
    .filter((value) => Number.isFinite(value))
    .sort((left, right) => left - right);

  if (sorted.length === 0) {
    return {
      samples: 0,
      min_ms: 0,
      avg_ms: 0,
      p50_ms: 0,
      p90_ms: 0,
      p95_ms: 0,
      p99_ms: 0,
      max_ms: 0,
    };
  }

  const sum = sorted.reduce((total, value) => total + value, 0);
  return {
    samples: sorted.length,
    min_ms: formatMs(sorted[0]),
    avg_ms: formatMs(sum / sorted.length),
    p50_ms: formatMs(percentile(sorted, 50)),
    p90_ms: formatMs(percentile(sorted, 90)),
    p95_ms: formatMs(percentile(sorted, 95)),
    p99_ms: formatMs(percentile(sorted, 99)),
    max_ms: formatMs(sorted[sorted.length - 1]),
  };
}

function loadContexts(filePath) {
  const raw = readFileSync(filePath, "utf8").trim();
  if (!raw) {
    throw new Error(`${filePath} does not contain any contexts`);
  }

  if (raw.startsWith("[")) {
    const parsed = JSON.parse(raw);
    if (!Array.isArray(parsed)) {
      throw new Error("RESOLVE_CONTEXTS_FILE JSON must be an array of objects");
    }
    return parsed;
  }

  return raw
    .split(/\r?\n/)
    .filter(Boolean)
    .map((line, index) => {
      try {
        return JSON.parse(line);
      } catch (error) {
        throw new Error(`Invalid JSON on context line ${index + 1}: ${error.message}`);
      }
    });
}

function generateContexts({
  dimensions,
  cardinality,
  poolSize,
  includeTargetingKey,
  targetingKeyPrefix,
}) {
  const contexts = new Array(poolSize);
  for (let index = 0; index < poolSize; index += 1) {
    const context = {};

    for (let dimIndex = 0; dimIndex < dimensions.length; dimIndex += 1) {
      const valueIndex = (index * (dimIndex + 1) + dimIndex) % cardinality;
      context[dimensions[dimIndex]] = `v${valueIndex}`;
    }

    if (includeTargetingKey) {
      context.targetingKey = `${targetingKeyPrefix}-${index % cardinality}`;
    }

    contexts[index] = context;
  }
  return contexts;
}

function describeResult(result) {
  if (result === null) {
    return "null";
  }

  if (Array.isArray(result)) {
    return `array(${result.length})`;
  }

  if (typeof result === "object") {
    return `object(${Object.keys(result).length} keys)`;
  }

  return `${typeof result}(${String(result)})`;
}

async function main() {
  if (process.argv.includes("--help") || process.argv.includes("-h")) {
    printHelp();
    return;
  }

  const endpoint = process.env.SUPERPOSITION_ENDPOINT || DEFAULT_ENDPOINT;
  const token = process.env.SUPERPOSITION_TOKEN || DEFAULT_TOKEN;
  const orgId = requireEnv("SUPERPOSITION_ORG_ID", "ORG_ID");
  const workspaceId = requireEnv("SUPERPOSITION_WORKSPACE_ID", "WORKSPACE_ID");
  const mode = process.env.RESOLVE_MODE || "all";
  const validModes = new Set(["all", "number", "string", "boolean", "object"]);
  if (!validModes.has(mode)) {
    throw new Error("RESOLVE_MODE must be one of: all, number, string, boolean, object");
  }

  const resolveKey = process.env.RESOLVE_KEY;
  if (mode !== "all" && !resolveKey) {
    throw new Error("RESOLVE_KEY is required when RESOLVE_MODE is not all");
  }

  const iterations = parseInteger("RESOLVE_ITERATIONS", 10_000, { min: 1 });
  const warmupIterations = parseInteger(
    "RESOLVE_WARMUP",
    Math.min(1_000, iterations),
    { min: 0 },
  );
  const concurrency = parseInteger("RESOLVE_CONCURRENCY", 1, { min: 1 });
  const latencySampleLimit = parseInteger("RESOLVE_LATENCY_SAMPLES", 50_000, {
    min: 1,
  });
  const dimensions = (process.env.RESOLVE_DIMENSIONS || "city,tenant,user_segment")
    .split(",")
    .map((dimension) => dimension.trim())
    .filter(Boolean);
  const cardinality = parseInteger("RESOLVE_CARDINALITY", 1_000, { min: 1 });
  const includeTargetingKey = parseBoolean("RESOLVE_INCLUDE_TARGETING_KEY", false);
  const targetingKeyPrefix =
    process.env.RESOLVE_TARGETING_KEY_PREFIX || "bench-user";
  const contextFile = process.env.RESOLVE_CONTEXTS_FILE;
  const generatedPoolSize = parseInteger(
    "RESOLVE_CONTEXT_POOL_SIZE",
    Math.min(iterations, 50_000),
    { min: 1 },
  );
  const defaultValue = parseDefaultValue(mode);

  const contexts = contextFile
    ? loadContexts(contextFile)
    : generateContexts({
        dimensions,
        cardinality,
        poolSize: generatedPoolSize,
        includeTargetingKey,
        targetingKeyPrefix,
      });

  if (contexts.length === 0) {
    throw new Error("At least one context is required");
  }

  const provider = new SuperpositionProvider({
    endpoint,
    token,
    org_id: orgId,
    workspace_id: workspaceId,
  });

  console.log("Starting Superposition provider resolve benchmark");
  console.table({
    endpoint,
    org_id: orgId,
    workspace_id: workspaceId,
    mode,
    key: resolveKey || "(all config)",
    iterations,
    warmup_iterations: warmupIterations,
    concurrency,
    contexts: contexts.length,
    context_source: contextFile || "generated",
    dimensions: contextFile ? "(from file)" : dimensions.join(","),
    cardinality: contextFile ? "(from file)" : cardinality,
  });

  const initStart = performance.now();
  await OpenFeature.setProviderAndWait(provider);
  const initMs = performance.now() - initStart;
  const client = OpenFeature.getClient();

  async function resolveOne(context) {
    switch (mode) {
      case "all":
        return provider.resolveAllConfigDetails({}, context);
      case "boolean":
        return client.getBooleanValue(resolveKey, defaultValue, context);
      case "number":
        return client.getNumberValue(resolveKey, defaultValue, context);
      case "object":
        return client.getObjectValue(resolveKey, defaultValue, context);
      case "string":
      default:
        return client.getStringValue(resolveKey, defaultValue, context);
    }
  }

  for (let index = 0; index < warmupIterations; index += 1) {
    await resolveOne(contexts[index % contexts.length]);
  }

  const latencySampleEvery = Math.max(
    1,
    Math.floor(iterations / latencySampleLimit),
  );
  const latencies = [];
  let errors = 0;
  let nextIndex = 0;
  let lastResult;

  async function worker() {
    while (true) {
      const index = nextIndex;
      nextIndex += 1;

      if (index >= iterations) {
        break;
      }

      const context = contexts[index % contexts.length];
      const shouldSample = index % latencySampleEvery === 0;
      const start = shouldSample ? performance.now() : 0;

      try {
        lastResult = await resolveOne(context);
      } catch (error) {
        errors += 1;
        if (errors <= 5) {
          console.error("Resolve error:", error);
        }
      } finally {
        if (shouldSample) {
          latencies.push(performance.now() - start);
        }
      }
    }
  }

  const runStart = performance.now();
  await Promise.all(Array.from({ length: concurrency }, () => worker()));
  const totalMs = performance.now() - runStart;
  const latencySummary = summarizeLatencies(latencies);

  console.log("\nBenchmark result");
  console.table({
    init_ms: formatMs(initMs),
    total_ms: formatMs(totalMs),
    ops_per_sec: Math.round((iterations / totalMs) * 1000),
    errors,
    sample_result: describeResult(lastResult),
    ...latencySummary,
  });

  await OpenFeature.close();
}

main().catch(async (error) => {
  console.error(error);
  await OpenFeature.close();
  process.exit(1);
});
