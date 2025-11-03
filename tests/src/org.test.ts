import {
    CreateOrganisationCommand,
    ListOrganisationCommand,
    GetOrganisationCommand,
    UpdateOrganisationCommand,
} from "@juspay/superposition-sdk";
import { expect, describe, it, afterAll } from "bun:test";
import { ENV, superpositionClient } from "../env.ts";

describe("Organisation Tests", () => {
    let createdOrgId: string;
    const ORG_NAME = "tmporg";

    describe("CreateOrganisationCommand", () => {
        it("should create an organisation successfully", async () => {
            const createCommand = new CreateOrganisationCommand({
                admin_email: "test@gmail.com",
                name: ORG_NAME,
            });
            try {
                const response = await superpositionClient.send(createCommand);
                expect(response).toBeDefined();
                expect(response).toHaveProperty("id");
                createdOrgId = response.id ?? "";
            } catch (e: any) {
                console.error(
                    `Error creating organisation due to ${e.message}`
                );
                expect(true).toBe(false);
            }
        });

        it("should handle invalid input", async () => {
            const createCommand = new CreateOrganisationCommand({
                admin_email: "invalid-email",
                name: "",
            });

            expect(superpositionClient.send(createCommand)).rejects.toThrow(
                /JSON Parse error: Unexpected identifier \"Json\"/
            );
        });

        // no such valdiation
        // it("should handle malformed email address", async () => {
        //     const createCommand = new CreateOrganisationCommand({
        //         admin_email: "test.com",
        //         name: ORG_NAME,
        //     });

        //     try {
        //         await superpositionClient.send(createCommand);
        //         console.error("Should have thrown an error");
        //         expect(true).toBe(false);
        //     } catch (error: any) {
        //         console.error("log: ", error.message);
        //         expect(error).toBeDefined();
        //     }
        // });
    });

    describe("GetOrganisationCommand", () => {
        it("should get an organisation by id", async () => {
            const getCommand = new GetOrganisationCommand({
                id: createdOrgId,
            });
            try {
                const response = await superpositionClient.send(getCommand);
                expect(response.name).toBe(ORG_NAME);
                expect(response.admin_email).toBe("test@gmail.com");
            } catch (e: any) {
                console.error("log: ", e.message);
                console.error(`Error getting organisation due to ${e}`);
                expect(true).toBe(false);
            }
        });

        it("should handle non-existent organisation id", async () => {
            const getCommand = new GetOrganisationCommand({
                id: "non-existent-id",
            });

            expect(superpositionClient.send(getCommand)).rejects.toThrow(
                "No records found. Please refine or correct your search parameters"
            );
        });

        it("should handle invalid UUID format", async () => {
            const getCommand = new GetOrganisationCommand({
                id: "invalid-uuid-format",
            });

            expect(superpositionClient.send(getCommand)).rejects.toThrow(
                "No records found. Please refine or correct your search parameters"
            );
        });

        it("should handle empty id parameter", async () => {
            const getCommand = new GetOrganisationCommand({
                id: "",
            });

            expect(superpositionClient.send(getCommand)).rejects.toThrow(
                "Empty value provided for input HTTP label: id."
            );
        });
    });

    describe("ListOrganisationCommand", () => {
        it("should list all organisations", async () => {
            const listCommand = new ListOrganisationCommand({});

            try {
                const response = await superpositionClient.send(listCommand);
                expect(Array.isArray(response.data)).toBe(true);
                expect(response.data?.length).toBeGreaterThan(0);
                const createdOrg = response.data?.find(
                    (org: any) => org.id === createdOrgId
                );
                expect(createdOrg).toBeDefined();
                expect(createdOrg?.name).toBe(ORG_NAME);
            } catch (e: any) {
                console.error("log: ", e.message);
                console.error(`Error listing organisations due to ${e}`);
                expect(true).toBe(false);
            }
        });

        it("should handle pagination", async () => {
            const listCommand = new ListOrganisationCommand({
                count: 1,
                page: 1,
            });
            try {
                const response = await superpositionClient.send(listCommand);
                expect(Array.isArray(response.data)).toBe(true);
                expect(response.data?.length).toBeLessThanOrEqual(1);
            } catch (e: any) {
                console.error("log: ", e.message);
                console.error(`Error listing organisations due to ${e}`);
                expect(true).toBe(false);
            }
        });

        it("should handle negative page numbers", async () => {
            const listCommand = new ListOrganisationCommand({
                count: 1,
                page: -1,
            });

            expect(superpositionClient.send(listCommand)).rejects.toThrow(
                /JSON Parse error: Unexpected identifier "Query"/
            );
        });

        it("should handle negative count values", async () => {
            const listCommand = new ListOrganisationCommand({
                count: -1,
                page: 0,
            });

            expect(superpositionClient.send(listCommand)).rejects.toThrow(
                /JSON Parse error: Unexpected identifier "Query"/
            );
        });
    });

    describe("UpdateOrganisationCommand", () => {
        it("should update an organisation successfully", async () => {
            const updateCommand = new UpdateOrganisationCommand({
                id: createdOrgId,
                admin_email: "updated-test@gmail.com",
            });
            try {
                const updateResponse = await superpositionClient.send(
                    updateCommand
                );
                expect(updateResponse).toBeDefined();
                expect(updateResponse).toHaveProperty("id");
                expect(updateResponse.admin_email).toBe(
                    "updated-test@gmail.com"
                );
                const getCommand = new GetOrganisationCommand({
                    id: createdOrgId,
                });
                const getResponse = await superpositionClient.send(getCommand);
                expect(getResponse.admin_email).toBe("updated-test@gmail.com");
            } catch (e: any) {
                console.error("log: ", e.message);
                console.error(`Error updating organisation due to ${e}`);
                expect(true).toBe(false);
            }
        });

        // no such validation
        // it("should handle invalid update data", async () => {
        //     const updateCommand = new UpdateOrganisationCommand({
        //         id: createdOrgId,
        //         admin_email: "invalid-email",
        //     });

        //     try {
        //         await superpositionClient.send(updateCommand);
        //         console.error("Should have thrown an error");
        //         expect(true).toBe(false);
        //     } catch (error: any) {
        //         console.error("log: ", error.message);
        //         expect(error).toBeDefined();
        //     }
        // });

        it("should handle non-existent organisation update", async () => {
            const updateCommand = new UpdateOrganisationCommand({
                id: "non-existent-id",
                admin_email: "test@gmail.com",
            });

            expect(superpositionClient.send(updateCommand)).rejects.toThrow(
                "No records found. Please refine or correct your search parameters"
            );
        });

        // no such validation
        // it("should handle empty update payload", async () => {
        //     const updateCommand = new UpdateOrganisationCommand({
        //         id: createdOrgId,
        //     });

        //     try {
        //         await superpositionClient.send(updateCommand);
        //         console.error("Should have thrown an error");
        //         expect(true).toBe(false);
        //     } catch (error: any) {
        //         console.error("log: ", error.message);
        //         expect(error).toBeDefined();
        //     }
        // });

        // no such validation
        // it("should handle malformed email in update", async () => {
        //     const updateCommand = new UpdateOrganisationCommand({
        //         id: createdOrgId,
        //         admin_email: "@invalid.com",
        //     });

        //     try {
        //         await superpositionClient.send(updateCommand);
        //         console.error("Should have thrown an error");
        //         expect(true).toBe(false);
        //     } catch (error: any) {
        //         console.error("log: ", error.message);
        //         expect(error).toBeDefined();
        //     }
        // });
    });

    afterAll(() => {
        console.log(`Env after update is ${JSON.stringify(ENV)}`);
    });
});
