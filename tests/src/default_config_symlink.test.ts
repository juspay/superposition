import {
    CreateDefaultConfigCommand,
    UpdateDefaultConfigCommand,
    DeleteDefaultConfigCommand,
    GetDefaultConfigCommand,
    CreateContextCommand,
    DeleteContextCommand,
    CreateDimensionCommand,
    DeleteDimensionCommand,
    CreateExperimentCommand,
    DiscardExperimentCommand,
    GetConfigCommand,
    VariantType,
    type CreateDefaultConfigCommandOutput,
    type ExperimentResponse,
} from "@juspay/superposition-sdk";
import { superpositionClient, ENV } from "../env.ts";
import { describe, beforeAll, afterAll, test, expect } from "bun:test";

const TARGET = "symlink.target.count";
const LINK = "symlink_target_count";
const SECOND_LINK = "symlink.alias.count";
const GUARD_KEY = "symlink.guard.count";
const DIMENSION = "symlink.test.dimension";

const base = { workspace_id: ENV.workspace_id, org_id: ENV.org_id };
const created: string[] = [];
// Every context this suite creates. Without deleting these, `afterAll`'s
// delete of TARGET is refused ("already in use in contexts") and swallowed by
// the try/catch, which left the keys behind and made a second run of the suite
// fail at create - the suite was not idempotent.
const createdContextIds = new Set<string>();
const createdExperimentIds = new Set<string>();

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

async function createContext(
    dimensionValue: string,
    override: Record<string, unknown>,
): Promise<string> {
    const response = await superpositionClient.send(
        new CreateContextCommand({
            ...base,
            request: {
                context: { [DIMENSION]: dimensionValue },
                override: override as any,
                description: `symlink test context ${dimensionValue}`,
                change_reason: "test",
            },
        }),
    );
    if (response.id) {
        createdContextIds.add(response.id);
    }
    return response.override_id as string;
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

        // An ordinary key the conversion-guard tests below use: one of them
        // repoints TARGET at it, another converts it. Created here rather than
        // inside a test so the later tests do not depend on an earlier one
        // having created it.
        await superpositionClient.send(
            new CreateDefaultConfigCommand({
                ...base,
                key: GUARD_KEY,
                value: 4,
                schema: { type: "integer", minimum: 0, maximum: 10 },
                description: "symlink conversion guard",
                change_reason: "test setup",
            }),
        );
        created.push(GUARD_KEY);
    });

    afterAll(async () => {
        // Experiments hold variant overrides on these keys, contexts hold
        // overrides on them, and a target cannot be deleted while a link points
        // at it - so the teardown runs experiments, then contexts, then the
        // default configs in reverse creation order (links before targets), then
        // the dimension.
        for (const id of createdExperimentIds) {
            try {
                await superpositionClient.send(
                    new DiscardExperimentCommand({
                        ...base,
                        id,
                        change_reason: "test cleanup",
                    }),
                );
            } catch (error) {
                console.log(`cleanup failed for experiment ${id}:`, error);
            }
        }

        for (const id of createdContextIds) {
            try {
                await superpositionClient.send(
                    new DeleteContextCommand({ ...base, id }),
                );
            } catch (error) {
                console.log(`cleanup failed for context ${id}:`, error);
            }
        }

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
        await createContext("on", { [LINK]: 7 });

        const config = await superpositionClient.send(new GetConfigCommand({ ...base }));
        const overrides = Object.values(config.overrides ?? {});
        const stored = overrides.find((o: any) => o[TARGET] === 7) as any;

        expect(stored).toBeDefined();
        expect(stored[TARGET]).toBe(7);
        expect(stored[LINK]).toBe(7);
    });

    test("the link and the target produce byte-identical override ids", async () => {
        // The override id is a hash of the override map's contents, so an
        // override written through the link must hash to exactly what the same
        // override written against the target hashes to - otherwise the same
        // semantic override would be stored twice under two ids and `reduce`
        // could never merge them.
        const viaLink = await createContext("byte-equal-a", { [LINK]: 6 });
        const viaTarget = await createContext("byte-equal-b", { [TARGET]: 6 });

        expect(viaLink).toBeDefined();
        expect(viaLink).toBe(viaTarget);
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

    test("two links to one target both resolve to it", async () => {
        // LINK and SECOND_LINK now both point at TARGET (the second was
        // flattened through the first). Both names must carry the target's
        // value in the evaluated config, and under the override too.
        const first = await superpositionClient.send(
            new GetDefaultConfigCommand({ ...base, key: LINK }),
        );
        const second = await superpositionClient.send(
            new GetDefaultConfigCommand({ ...base, key: SECOND_LINK }),
        );
        expect(first.symlink_to).toBe(TARGET);
        expect(second.symlink_to).toBe(TARGET);

        const config = await superpositionClient.send(new GetConfigCommand({ ...base }));
        expect(config.default_configs?.[LINK]).toBe(8);
        expect(config.default_configs?.[SECOND_LINK]).toBe(8);
        expect(config.default_configs?.[TARGET]).toBe(8);

        const overridden = Object.values(config.overrides ?? {}).find(
            (o: any) => o[TARGET] === 7,
        ) as any;
        expect(overridden[LINK]).toBe(7);
        expect(overridden[SECOND_LINK]).toBe(7);
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

    test("a marker that is not the boolean true is refused outright", async () => {
        // This is the authorization-denial case the design's test list calls
        // for, in the form this environment can actually assert. AUTH_Z_PROVIDER
        // is DISABLED here, so a 403 cannot be provoked; what *can* be asserted
        // is that the write which used to slip past the symlink target's
        // authorization check is now refused. `->>` unquotes in Postgres, so the
        // JSON string "true" matched the read path's symlink predicate while
        // Rust's `is_symlink_schema` saw an ordinary key - the write therefore
        // authorized only the name the caller chose and skipped the target,
        // while the read path published the target's value under that name.
        await expect(
            superpositionClient.send(
                new CreateDefaultConfigCommand({
                    ...base,
                    key: "symlink.string.marker",
                    value: TARGET,
                    schema: { type: "string", "x-superposition-symlink": "true" },
                    description: "should be refused",
                    change_reason: "test",
                }),
            ),
        ).rejects.toThrow(/must be the boolean true/);

        // And the same through the update path, which shares the gate.
        await expect(
            superpositionClient.send(
                new UpdateDefaultConfigCommand({
                    ...base,
                    key: LINK,
                    value: TARGET,
                    schema: { "x-superposition-symlink": 1 },
                    change_reason: "test",
                }),
            ),
        ).rejects.toThrow(/must be the boolean true/);
    });

    test("an experiment variant override written against the link is stored against the target", async () => {
        const experiment: ExperimentResponse = await superpositionClient.send(
            new CreateExperimentCommand({
                ...base,
                name: `symlink-variant-normalization-${Date.now()}`,
                context: { [DIMENSION]: "experiment" },
                variants: [
                    {
                        id: "control",
                        variant_type: VariantType.CONTROL,
                        overrides: { [LINK]: 2 },
                    },
                    {
                        id: "test",
                        variant_type: VariantType.EXPERIMENTAL,
                        overrides: { [LINK]: 5 },
                    },
                ],
                description: "symlink variant normalization",
                change_reason: "test",
            }),
        );
        if (experiment.id) {
            createdExperimentIds.add(experiment.id);
        }

        expect(experiment.override_keys).toContain(TARGET);
        expect(experiment.override_keys).not.toContain(LINK);
        for (const variant of experiment.variants ?? []) {
            expect(Object.keys(variant.overrides ?? {})).toContain(TARGET);
            expect(Object.keys(variant.overrides ?? {})).not.toContain(LINK);
        }
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

    test("converting a key that symlinks point at is refused", async () => {
        // Otherwise `LINK -> TARGET` would become a link to a link: the chain
        // the design rules out, and the route to a cycle.
        await expect(
            superpositionClient.send(
                new UpdateDefaultConfigCommand({
                    ...base,
                    key: TARGET,
                    value: GUARD_KEY,
                    schema: { "x-superposition-symlink": true },
                    change_reason: "test",
                }),
            ),
        ).rejects.toThrow(new RegExp(LINK));
    });

    test("converting a key that is overridden in a context is refused", async () => {
        // The core invariant: a link holds no value of its own, so the stored
        // override - which names GUARD_KEY, not its target - would stop
        // agreeing with the target, silently, and could no longer be reduced.
        await createContext("guard", { [GUARD_KEY]: 4 });

        await expect(
            superpositionClient.send(
                new UpdateDefaultConfigCommand({
                    ...base,
                    key: GUARD_KEY,
                    value: TARGET,
                    schema: { "x-superposition-symlink": true },
                    change_reason: "test",
                }),
            ),
        ).rejects.toThrow(/overridden in context/);
    });
});
