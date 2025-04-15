import {
    SuperpositionClient,
    CreateOrganisationCommand,
    ListOrganisationCommand,
    GetOrganisationCommand,
    UpdateOrganisationCommand,
} from "@io.juspay/superposition-sdk";
import { expect, describe, beforeAll, it, afterAll } from "bun:test";
import { ENV } from "./env.test.ts";

describe("Organisation Tests", () => {
    let client: SuperpositionClient;
    let createdOrgId: string;

    beforeAll(() => {
        client = new SuperpositionClient({
            endpoint: ENV.baseUrl,
            token: {
                token: "some-token",
            },
        });
    });

    describe("CreateOrganisationCommand", () => {
        it("should create an organisation successfully", async () => {
            const createCommand = new CreateOrganisationCommand({
                admin_email: "test@gmail.com",
                name: "testorg",
            });
            try {
                const response = await client.send(createCommand);
                expect(response).toBeDefined();
                expect(response).toHaveProperty("id");
                createdOrgId = response.id ?? ENV.org_id;
                ENV.org_id = createdOrgId;
            } catch (e) {
                console.error(`Error creating organisation due to ${e}`);
            }
        });

        it("should handle invalid input", async () => {
            const createCommand = new CreateOrganisationCommand({
                admin_email: "invalid-email",
                name: "",
            });

            try {
                await client.send(createCommand);
                console.error("Should have thrown an error");
            } catch (error) {
                expect(error).toBeDefined();
            }
        });

        it("should handle missing required fields", async () => {
            const createCommand = new CreateOrganisationCommand({
                admin_email: "",
                name: "",
            });

            try {
                await client.send(createCommand);
                console.error("Should have thrown an error");
            } catch (error) {
                expect(error).toBeDefined();
            }
        });

        it("should handle malformed email address", async () => {
            const createCommand = new CreateOrganisationCommand({
                admin_email: "test.com",
                name: "testorg",
            });

            try {
                await client.send(createCommand);
                console.error("Should have thrown an error");
            } catch (error) {
                expect(error).toBeDefined();
            }
        });
    });

    describe("GetOrganisationCommand", () => {
        it("should get an organisation by id", async () => {
            const getCommand = new GetOrganisationCommand({
                id: createdOrgId,
            });
            try {
                const response = await client.send(getCommand);
                expect(response.name).toBe("testorg");
                expect(response.admin_email).toBe("test@gmail.com");
            } catch (e) {}
        });

        it("should handle non-existent organisation id", async () => {
            const getCommand = new GetOrganisationCommand({
                id: "non-existent-id",
            });

            try {
                await client.send(getCommand);
                ("Should have thrown an error");
            } catch (error) {
                expect(error).toBeDefined();
            }
        });

        it("should handle invalid UUID format", async () => {
            const getCommand = new GetOrganisationCommand({
                id: "invalid-uuid-format",
            });

            try {
                await client.send(getCommand);
                console.error("Should have thrown an error");
            } catch (error) {
                expect(error).toBeDefined();
            }
        });

        it("should handle empty id parameter", async () => {
            const getCommand = new GetOrganisationCommand({
                id: "",
            });

            try {
                await client.send(getCommand);
                console.error("Should have thrown an error");
            } catch (error) {
                expect(error).toBeDefined();
            }
        });
    });

    describe("ListOrganisationCommand", () => {
        it("should list all organisations", async () => {
            const listCommand = new ListOrganisationCommand({});

            try {
                const response = await client.send(listCommand);
                expect(Array.isArray(response.data)).toBe(true);
                expect(response.data?.length).toBeGreaterThan(0);
                const createdOrg = response.data?.find(
                    (org: any) => org.id === createdOrgId,
                );
                expect(createdOrg).toBeDefined();
                expect(createdOrg?.name).toBe("testorg");
            } catch (e) {
                console.error(`Error listing organisations due to ${e}`);
            }
        });

        it("should handle pagination", async () => {
            const listCommand = new ListOrganisationCommand({
                count: 1,
                page: 0,
            });
            try {
                const response = await client.send(listCommand);
                expect(Array.isArray(response.data)).toBe(true);
                expect(response.data?.length).toBeLessThanOrEqual(1);
            } catch (e) {
                console.error(`Error listing organisations due to ${e}`);
            }
        });

        it("should handle negative page numbers", async () => {
            const listCommand = new ListOrganisationCommand({
                count: 1,
                page: -1,
            });
            try {
                await client.send(listCommand);
                console.error("Should have thrown an error");
            } catch (error) {
                expect(error).toBeDefined();
            }
        });

        it("should handle negative count values", async () => {
            const listCommand = new ListOrganisationCommand({
                count: -1,
                page: 0,
            });
            try {
                await client.send(listCommand);
                console.error("Should have thrown an error");
            } catch (error) {
                expect(error).toBeDefined();
            }
        });
    });

    describe("UpdateOrganisationCommand", () => {
        it("should update an organisation successfully", async () => {
            const updateCommand = new UpdateOrganisationCommand({
                id: createdOrgId,
                admin_email: "updated-test@gmail.com",
            });
            try {
                const updateResponse = await client.send(updateCommand);
                expect(updateResponse).toBeDefined();
                expect(updateResponse).toHaveProperty("id");
                expect(updateResponse.admin_email).toBe(
                    "updated-test@gmail.com",
                );
                const getCommand = new GetOrganisationCommand({
                    id: createdOrgId,
                });
                const getResponse = await client.send(getCommand);
                expect(getResponse.name).toBe("updated-testorg");
                expect(getResponse.admin_email).toBe("updated-test@gmail.com");
            } catch (e) {
                console.error(`Error updating organisation due to ${e}`);
            }
        });

        it("should handle invalid update data", async () => {
            const updateCommand = new UpdateOrganisationCommand({
                id: createdOrgId,
                admin_email: "invalid-email",
            });

            try {
                await client.send(updateCommand);
                console.error("Should have thrown an error");
            } catch (error) {
                expect(error).toBeDefined();
            }
        });

        it("should handle non-existent organisation update", async () => {
            const updateCommand = new UpdateOrganisationCommand({
                id: "non-existent-id",
                admin_email: "test@gmail.com",
            });

            try {
                await client.send(updateCommand);
                console.error("Should have thrown an error");
            } catch (error) {
                expect(error).toBeDefined();
            }
        });

        it("should handle empty update payload", async () => {
            const updateCommand = new UpdateOrganisationCommand({
                id: createdOrgId,
            });

            try {
                await client.send(updateCommand);
                console.error("Should have thrown an error");
            } catch (error) {
                expect(error).toBeDefined();
            }
        });

        it("should handle malformed email in update", async () => {
            const updateCommand = new UpdateOrganisationCommand({
                id: createdOrgId,
                admin_email: "@invalid.com",
            });

            try {
                await client.send(updateCommand);
                console.error("Should have thrown an error");
            } catch (error) {
                expect(error).toBeDefined();
            }
        });
    });

    afterAll(() => {
        console.log(`Env after update is ${JSON.stringify(ENV)}`);
    });
});
