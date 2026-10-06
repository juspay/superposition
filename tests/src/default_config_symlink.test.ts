import {
    CreateDefaultConfigCommand,
    UpdateDefaultConfigCommand,
    DeleteDefaultConfigCommand,
    GetDefaultConfigCommand,
    CreateContextCommand,
    CreateDimensionCommand,
    DeleteDimensionCommand,
    GetConfigCommand,
    type CreateDefaultConfigCommandOutput,
} from "@juspay/superposition-sdk";
import { superpositionClient, ENV } from "../env.ts";
import { describe, beforeAll, afterAll, test, expect } from "bun:test";

const TARGET = "symlink.target.count";
const LINK = "symlink_target_count";
const SECOND_LINK = "symlink.alias.count";
const DIMENSION = "symlink.test.dimension";

const base = { workspace_id: ENV.workspace_id, org_id: ENV.org_id };
const created: string[] = [];

async function createTarget() {
    await superpositionClient.send(
        new CreateDefaultConfigCommand({
            ...base,
            key: TARGET,
            value: 3,
            schema: { type: "integer", minimum: 0, maximum: 10 },
            description: "symlink test target",
            change_reason: "test setup",
        }),
    );
    created.push(TARGET);
}

async function createLink(
    key: string,
    target: string,
): Promise<CreateDefaultConfigCommandOutput> {
    const response = await superpositionClient.send(
        new CreateDefaultConfigCommand({
            ...base,
            key,
            value: target,
            schema: { "x-superposition-symlink": true },
            description: `alias of ${target}`,
            change_reason: "test setup",
        }),
    );
    created.push(key);
    return response;
}

describe("Default Config Symlinks", () => {
    // The override tests below key their contexts off this dimension, which
    // does not exist in the base test workspace, so it is created here and
    // torn down afterwards (tests/src/context.test.ts follows this pattern).
    beforeAll(async () => {
        await superpositionClient.send(
            new CreateDimensionCommand({
                ...base,
                dimension: DIMENSION,
                schema: { type: "string" },
                position: 2,
                description: "Dimension for default-config symlink tests",
                change_reason: "test setup",
            }),
        );
    });

    afterAll(async () => {
        // Links first: a target cannot be deleted while a link points at it.
        for (const key of [...created].reverse()) {
            try {
                await superpositionClient.send(
                    new DeleteDefaultConfigCommand({ ...base, key }),
                );
            } catch (error) {
                console.log(`cleanup failed for ${key}:`, error);
            }
        }

        try {
            await superpositionClient.send(
                new DeleteDimensionCommand({ ...base, dimension: DIMENSION }),
            );
        } catch (error) {
            console.log(`cleanup failed for dimension ${DIMENSION}:`, error);
        }
    });

    test("a link resolves to the target's value and schema, and reports symlink_to", async () => {
        await createTarget();
        await createLink(LINK, TARGET);

        const got = await superpositionClient.send(
            new GetDefaultConfigCommand({ ...base, key: LINK }),
        );

        expect(got.symlink_to).toBe(TARGET);
        expect(got.value).toBe(3);
        expect(got.schema).toMatchObject({ type: "integer" });
        expect(got.schema?.["x-superposition-symlink"]).toBeUndefined();
    });

    test("both names appear in the evaluated config", async () => {
        const config = await superpositionClient.send(new GetConfigCommand({ ...base }));

        expect(config.default_configs?.[TARGET]).toBe(3);
        expect(config.default_configs?.[LINK]).toBe(3);
    });

    test("an override written against the link lands on the target", async () => {
        await superpositionClient.send(
            new CreateContextCommand({
                ...base,
                request: {
                    context: { [DIMENSION]: "on" },
                    override: { [LINK]: 7 },
                    description: "override via the link",
                    change_reason: "test",
                },
            }),
        );

        const config = await superpositionClient.send(new GetConfigCommand({ ...base }));
        const overrides = Object.values(config.overrides ?? {});
        const stored = overrides.find((o: any) => o[TARGET] === 7) as any;

        expect(stored).toBeDefined();
        expect(stored[TARGET]).toBe(7);
        expect(stored[LINK]).toBe(7);
    });

    test("a value update on the link moves the target", async () => {
        await superpositionClient.send(
            new UpdateDefaultConfigCommand({
                ...base,
                key: LINK,
                value: 8,
                change_reason: "write through the link",
            }),
        );

        const target = await superpositionClient.send(
            new GetDefaultConfigCommand({ ...base, key: TARGET }),
        );
        expect(target.value).toBe(8);
        expect(target.symlink_to).toBeUndefined();
    });

    test("deleting the target is refused while a link points at it", async () => {
        await expect(
            superpositionClient.send(new DeleteDefaultConfigCommand({ ...base, key: TARGET })),
        ).rejects.toThrow(new RegExp(LINK));
    });

    test("a link to a link is flattened to the concrete key", async () => {
        const response = await createLink(SECOND_LINK, LINK);
        expect(response.symlink_to).toBe(TARGET);
    });

    test("a symlink cannot carry a validation function", async () => {
        await expect(
            superpositionClient.send(
                new CreateDefaultConfigCommand({
                    ...base,
                    key: "symlink.rejected",
                    value: TARGET,
                    schema: { "x-superposition-symlink": true },
                    value_validation_function_name: "any_function",
                    description: "should be refused",
                    change_reason: "test",
                }),
            ),
        ).rejects.toThrow(/validation or compute functions/);
    });

    test("an override naming both a link and its target is refused", async () => {
        // The override values must differ (1 vs. 2): apply_symlink_map only rejects
        // two keys that resolve to the same target when their values disagree — if
        // both named LINK and TARGET with the same value, the rewrite would collapse
        // them into one entry without error, and this test would stop exercising the
        // refusal it's named for.
        await expect(
            superpositionClient.send(
                new CreateContextCommand({
                    ...base,
                    request: {
                        context: { [DIMENSION]: "collide" },
                        override: { [LINK]: 1, [TARGET]: 2 },
                        description: "colliding override",
                        change_reason: "test",
                    },
                }),
            ),
        ).rejects.toThrow(/resolve to the same config key/);
    });
});
