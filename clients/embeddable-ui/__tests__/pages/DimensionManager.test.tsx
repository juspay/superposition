import { render, screen, waitFor } from "@testing-library/react";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { DimensionManager } from "../../src/pages/DimensionManager";
import { AlertProvider } from "../../src/providers/AlertProvider";
import { SuperpositionUIProvider } from "../../src/providers/SuperpositionUIProvider";

const testConfig = {
    apiBaseUrl: "https://test.com",
    orgId: "org",
    workspace: "ws",
};

const mockDimensions = {
    total_pages: 1,
    total_items: 2,
    data: [
        {
            dimension: "region",
            position: 0,
            created_at: "",
            created_by: "",
            schema: { type: "string" },
            value_validation_function_name: null,
            last_modified_at: "",
            last_modified_by: "",
            mandatory: true,
            dependency_graph: {},
            description: "Regular dimension",
            change_reason: "",
            value_compute_function_name: null,
            dimension_type: { REGULAR: {} },
        },
        {
            dimension: "cohort",
            position: 1,
            created_at: "",
            created_by: "",
            schema: { type: "string" },
            value_validation_function_name: null,
            last_modified_at: "",
            last_modified_by: "",
            mandatory: false,
            dependency_graph: {},
            description: "Local cohort dimension",
            change_reason: "",
            value_compute_function_name: null,
            dimension_type: { LOCAL_COHORT: "user_segment" },
        },
    ],
};

describe("DimensionManager", () => {
    const mockFetch = vi.fn();

    beforeEach(() => {
        vi.stubGlobal("fetch", mockFetch);
        mockFetch.mockResolvedValue({
            ok: true,
            status: 200,
            headers: new Headers({ "content-length": "500" }),
            json: () => Promise.resolve(mockDimensions),
        });
    });

    afterEach(() => {
        vi.restoreAllMocks();
    });

    it("renders object-shaped dimension types without raw object output", async () => {
        render(
            <SuperpositionUIProvider config={testConfig}>
                <AlertProvider>
                    <DimensionManager />
                </AlertProvider>
            </SuperpositionUIProvider>,
        );

        await waitFor(() => {
            expect(screen.getByText("REGULAR")).toBeDefined();
            expect(screen.getByText("LOCAL_COHORT:user_segment")).toBeDefined();
        });

        expect(document.body.textContent).not.toContain("[object Object]");
    });
});
