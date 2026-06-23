import {
    CreateWorkspaceCommand,
    MigrateWorkspaceSchemaCommand,
    WorkspaceStatus,
    ListDimensionsCommand,
    ListDefaultConfigsCommand,
    ListContextsCommand,
    GetConfigJsonCommand,
    ImportConfigJsonCommand,
    ImportConfigTomlCommand,
    type ImportConfigOutput,
    ImportStrategy,
    type ImportOnError,
} from "@juspay/superposition-sdk";
import { superpositionClient, ENV } from "../env.ts";
import { describe, test, expect, beforeAll } from "bun:test";

// Import with the `replace` strategy mirrors the *entire* workspace, so these tests run in
// their own dedicated workspace to avoid clobbering data created by other suites.

const IMPORT_WORKSPACE = "importtestws";
const suffix = Math.random().toString(36).substring(7);

const TIER = `imp_tier_${suffix}`;
const REGION = `imp_region_${suffix}`;
const RATE = `imp_rate_${suffix}`;
const FLAG = `imp_flag_${suffix}`;
const DRYRUN_KEY = `imp_dryrun_${suffix}`;
const TOML_KEY = `imp_toml_${suffix}`;

type ImportOpts = {
    strategy?: ImportStrategy;
    on_error?: ImportOnError;
    dry_run?: boolean;
};

async function importConfig(
    format: "toml" | "json",
    body: string,
    opts: ImportOpts = {},
): Promise<{ status: number; summary?: ImportConfigOutput; error?: unknown }> {
    const base = {
        workspace_id: IMPORT_WORKSPACE,
        org_id: ENV.org_id,
        ...opts,
    };
    const cmd =
        format === "json"
            ? new ImportConfigJsonCommand({ ...base, json_config: body })
            : new ImportConfigTomlCommand({ ...base, toml_config: body });
    try {
        const summary = await superpositionClient.send(cmd);
        return {
            status: summary.$metadata.httpStatusCode ?? 200,
            summary,
        };
    } catch (e: any) {
        return {
            status:
                e?.$metadata?.httpStatusCode ??
                e?.$response?.statusCode ??
                500,
            error: e,
        };
    }
}

// A self-consistent JSON config: contexts only reference dimensions/keys defined
// in the same document. `opts.includeFlag` lets a test drop one default-config.
function buildJsonConfig(opts: { includeFlag: boolean }): string {
    const defaultConfigs: Record<string, unknown> = {
        [RATE]: { value: 10, schema: { type: "number" } },
    };
    if (opts.includeFlag) {
        defaultConfigs[FLAG] = {
            value: { enabled: true, mode: "a", nested: { x: 1 } },
            schema: { type: "object" },
        };
    }
    return JSON.stringify({
        "default-configs": defaultConfigs,
        dimensions: {
            [TIER]: {
                position: 1,
                schema: { type: "string", enum: ["gold", "silver"] },
            },
            [REGION]: {
                position: 2,
                schema: { type: "string", enum: ["us", "eu"] },
            },
        },
        overrides: [{ _context_: { [TIER]: "gold" }, [RATE]: 20 }],
    });
}

async function listDefaultConfigKeys(): Promise<string[]> {
    const out = await superpositionClient.send(
        new ListDefaultConfigsCommand({
            workspace_id: IMPORT_WORKSPACE,
            org_id: ENV.org_id,
            count: 100,
            page: 1,
        }),
    );
    return (out.data ?? []).map((d) => d.key as string);
}

async function getDefaultConfigValue(key: string): Promise<any | undefined> {
    const out = await superpositionClient.send(
        new ListDefaultConfigsCommand({
            workspace_id: IMPORT_WORKSPACE,
            org_id: ENV.org_id,
            count: 100,
            page: 1,
        }),
    );
    return (out.data ?? []).find((d) => d.key === key)?.value;
}

async function listDimensionNames(): Promise<string[]> {
    const out = await superpositionClient.send(
        new ListDimensionsCommand({
            workspace_id: IMPORT_WORKSPACE,
            org_id: ENV.org_id,
            count: 100,
            page: 1,
        }),
    );
    return (out.data ?? []).map((d) => d.dimension as string);
}

async function countContexts(): Promise<number> {
    const out = await superpositionClient.send(
        new ListContextsCommand({
            workspace_id: IMPORT_WORKSPACE,
            org_id: ENV.org_id,
            count: 100,
            page: 1,
        }),
    );
    return (out.data ?? []).length;
}

beforeAll(async () => {
    // Dedicated workspace so `replace` strategy imports can't affect other suites.
    try {
        await superpositionClient.send(
            new CreateWorkspaceCommand({
                org_id: ENV.org_id,
                workspace_admin_email: "admin@example.com",
                workspace_name: IMPORT_WORKSPACE,
                workspace_status: WorkspaceStatus.ENABLED,
                allow_experiment_self_approval: true,
                auto_populate_control: false,
                enable_context_validation: true,
                enable_change_reason_validation: false,
            }),
        );
        console.log(`Created import test workspace: ${IMPORT_WORKSPACE}`);
    } catch (e: any) {
        // Already exists from a previous run — fine, reuse it.
        console.log(`Reusing import test workspace: ${e?.message ?? ""}`);
    }

    await superpositionClient.send(
        new MigrateWorkspaceSchemaCommand({
            org_id: ENV.org_id,
            workspace_name: IMPORT_WORKSPACE,
        }),
    );
});

describe("Config import - JSON", () => {
    test("upsert import creates dimensions, default-configs and contexts", async () => {
        const { status, summary } = await importConfig(
            "json",
            buildJsonConfig({ includeFlag: true }),
        );

        expect(status).toBe(200);
        expect(summary).toBeDefined();
        expect(summary!.strategy).toBe("upsert");
        expect(summary!.dry_run).toBe(false);
        expect(summary!.config_version).toBeDefined();
        expect(summary!.dimensions.created).toBeGreaterThanOrEqual(2);
        expect(summary!.default_configs.created).toBeGreaterThanOrEqual(2);
        expect(summary!.contexts.created).toBeGreaterThanOrEqual(1);

        const dims = await listDimensionNames();
        expect(dims).toContain(TIER);
        expect(dims).toContain(REGION);

        const keys = await listDefaultConfigKeys();
        expect(keys).toContain(RATE);
        expect(keys).toContain(FLAG);

        expect(await countContexts()).toBeGreaterThanOrEqual(1);
    });

    test("re-importing the same file updates instead of creating", async () => {
        const { status, summary } = await importConfig(
            "json",
            buildJsonConfig({ includeFlag: true }),
        );

        expect(status).toBe(200);
        expect(summary!.default_configs.created).toBe(0);
        expect(summary!.default_configs.updated).toBeGreaterThanOrEqual(2);
        expect(summary!.dimensions.updated).toBeGreaterThanOrEqual(2);
    });

    test("create_only skips entities that already exist", async () => {
        const { status, summary } = await importConfig(
            "json",
            buildJsonConfig({ includeFlag: true }),
            { strategy: ImportStrategy.CREATE_ONLY },
        );

        expect(status).toBe(200);
        expect(summary!.default_configs.created).toBe(0);
        expect(summary!.default_configs.updated).toBe(0);
        expect(summary!.default_configs.skipped).toBeGreaterThanOrEqual(2);
        expect(summary!.dimensions.skipped).toBeGreaterThanOrEqual(2);
    });

    test("upsert replaces object default-config values wholesale", async () => {
        const body = JSON.stringify({
            "default-configs": {
                [FLAG]: {
                    value: { mode: "b", nested: { y: 2 } },
                    schema: { type: "object" },
                },
            },
            dimensions: {},
            overrides: [],
        });

        const { status, summary } = await importConfig("json", body);

        expect(status).toBe(200);
        expect(summary!.default_configs.updated).toBeGreaterThanOrEqual(1);

        const value = await getDefaultConfigValue(FLAG);
        expect(value).toEqual({
            mode: "b",
            nested: { y: 2 },
        });
    });

    test("dry-run reports changes without persisting", async () => {
        const body = JSON.stringify({
            "default-configs": {
                [DRYRUN_KEY]: { value: 1, schema: { type: "number" } },
            },
            dimensions: {},
            overrides: [],
        });

        const { status, summary } = await importConfig("json", body, {
            dry_run: true,
        });

        expect(status).toBe(200);
        expect(summary!.dry_run).toBe(true);
        expect(summary!.default_configs.created).toBeGreaterThanOrEqual(1);
        // nothing committed, so no config version and the key must not exist
        expect(summary!.config_version).toBeUndefined();

        const keys = await listDefaultConfigKeys();
        expect(keys).not.toContain(DRYRUN_KEY);
    });

    test("replace strategy deletes entities absent from the file", async () => {
        // Drop FLAG from the document; replace strategy should remove it.
        const { status, summary } = await importConfig(
            "json",
            buildJsonConfig({ includeFlag: false }),
            { strategy: ImportStrategy.REPLACE },
        );

        expect(status).toBe(200);
        expect(summary!.strategy).toBe("replace");
        expect(summary!.default_configs.deleted).toBeGreaterThanOrEqual(1);

        const keys = await listDefaultConfigKeys();
        expect(keys).toContain(RATE);
        expect(keys).not.toContain(FLAG);
    });

    test("invalid body is rejected with a 4xx", async () => {
        const { status } = await importConfig("json", "{ not valid json ");
        expect(status).toBeGreaterThanOrEqual(400);
        expect(status).toBeLessThan(500);
    });

    test("context referencing an undeclared dimension is rejected", async () => {
        const body = JSON.stringify({
            "default-configs": {
                [RATE]: { value: 10, schema: { type: "number" } },
            },
            dimensions: {},
            overrides: [{ _context_: { nonexistent_dim: "x" }, [RATE]: 20 }],
        });
        const { status } = await importConfig("json", body);
        expect(status).toBeGreaterThanOrEqual(400);
        expect(status).toBeLessThan(500);
    });

    test("export via SDK can be re-imported (round-trip)", async () => {
        const exported = await superpositionClient.send(
            new GetConfigJsonCommand({
                workspace_id: IMPORT_WORKSPACE,
                org_id: ENV.org_id,
            }),
        );
        expect(exported.json_config).toBeDefined();

        const { status, summary } = await importConfig(
            "json",
            exported.json_config as string,
        );
        expect(status).toBe(200);
        // a faithful round-trip changes nothing new
        expect(summary!.default_configs.created).toBe(0);
        expect(summary!.dimensions.created).toBe(0);
    });
});

describe("Config import - TOML", () => {
    test("upsert import via the TOML endpoint", async () => {
        const toml = [
            "[default-configs]",
            `${TOML_KEY} = { value = 5, schema = { type = "number" } }`,
            "",
            "[dimensions]",
            `${TIER} = { position = 1, schema = { type = "string", enum = ["gold", "silver"] } }`,
            "",
            "[[overrides]]",
            `_context_ = { ${TIER} = "silver" }`,
            `${TOML_KEY} = 7`,
            "",
        ].join("\n");

        const { status, summary } = await importConfig("toml", toml);

        expect(status).toBe(200);
        expect(summary!.default_configs.created).toBeGreaterThanOrEqual(1);

        const keys = await listDefaultConfigKeys();
        expect(keys).toContain(TOML_KEY);
    });
});
