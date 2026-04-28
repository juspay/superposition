import { fireEvent, render, screen, waitFor } from "@testing-library/react";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { OverrideManager } from "../../src/pages/OverrideManager";
import { AlertProvider } from "../../src/providers/AlertProvider";
import { SuperpositionProvider } from "../../src/providers/SuperpositionProvider";

const testConfig = {
  apiBaseUrl: "https://test.com",
  orgId: "org",
  workspace: "ws",
};

const mockOverrides = {
  total_pages: 1,
  total_items: 2,
  data: [
    {
      id: "ctx-1",
      value: { region: "us-east-1", env: "prod" },
      override_id: "ovr-1",
      created_at: "2024-01-01T00:00:00Z",
      created_by: "admin",
      override: { "app.title": "US App" },
      last_modified_at: "2024-01-01T00:00:00Z",
      last_modified_by: "admin",
      weight: "100",
      description: "US override",
      change_reason: "init",
    },
    {
      id: "ctx-2",
      value: { region: "eu-west-1" },
      override_id: "ovr-2",
      created_at: "2024-01-01T00:00:00Z",
      created_by: "admin",
      override: { "app.title": "EU App" },
      last_modified_at: "2024-01-01T00:00:00Z",
      last_modified_by: "admin",
      weight: "50",
      description: "EU override",
      change_reason: "init",
    },
  ],
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
      description: "",
      change_reason: "",
      value_compute_function_name: null,
      dimension_type: "REGULAR",
    },
    {
      dimension: "env",
      position: 1,
      created_at: "",
      created_by: "",
      schema: { type: "string", enum: ["prod", "staging"] },
      value_validation_function_name: null,
      last_modified_at: "",
      last_modified_by: "",
      mandatory: false,
      dependency_graph: {},
      description: "",
      change_reason: "",
      value_compute_function_name: null,
      dimension_type: "REGULAR",
    },
  ],
};

const mockDefaultConfigs = {
  total_pages: 1,
  total_items: 2,
  data: [
    {
      key: "app.title",
      value: "Default title",
      created_at: "",
      created_by: "",
      schema: { type: "string" },
      value_validation_function_name: null,
      last_modified_at: "",
      last_modified_by: "",
      description: "",
      change_reason: "",
      value_compute_function_name: null,
    },
    {
      key: "feature.enabled",
      value: false,
      created_at: "",
      created_by: "",
      schema: { type: "boolean" },
      value_validation_function_name: null,
      last_modified_at: "",
      last_modified_by: "",
      description: "",
      change_reason: "",
      value_compute_function_name: null,
    },
  ],
};

function buildResponse(body: unknown) {
  return {
    ok: true,
    status: 200,
    headers: new Headers({ "content-length": "500" }),
    json: () => Promise.resolve(body),
  };
}

describe("OverrideManager", () => {
  const mockFetch = vi.fn();

  beforeEach(() => {
    vi.stubGlobal("fetch", mockFetch);
    mockFetch.mockImplementation((url: string, init?: RequestInit) => {
      if (init?.method === "PUT") {
        return Promise.resolve(
          buildResponse({
            id: "ctx-3",
            value: { region: "us-east-1", env: "prod" },
            override_id: "ovr-3",
            created_at: "2024-01-01T00:00:00Z",
            created_by: "admin",
            override: { "app.title": "Scoped App" },
            last_modified_at: "2024-01-01T00:00:00Z",
            last_modified_by: "admin",
            weight: "100",
            description: "Scoped override",
            change_reason: "test create",
          }),
        );
      }

      if (url.includes("/dimension")) {
        return Promise.resolve(buildResponse(mockDimensions));
      }

      if (url.includes("/default-config")) {
        return Promise.resolve(buildResponse(mockDefaultConfigs));
      }

      return Promise.resolve(buildResponse(mockOverrides));
    });
  });

  afterEach(() => {
    vi.restoreAllMocks();
  });

  it("renders override list", async () => {
    render(
      <SuperpositionProvider config={testConfig}>
        <AlertProvider>
          <OverrideManager />
        </AlertProvider>
      </SuperpositionProvider>,
    );

    await waitFor(() => {
      expect(screen.getByRole("heading", { name: "Overrides" })).toBeDefined();
      expect(screen.getByText("US override")).toBeDefined();
    });
  });

  it("filters overrides by scoped context", async () => {
    render(
      <SuperpositionProvider
        config={{
          ...testConfig,
          scope: { context: { region: "us-east-1" } },
        }}
      >
        <AlertProvider>
          <OverrideManager />
        </AlertProvider>
      </SuperpositionProvider>,
    );

    await waitFor(() => {
      expect(screen.getByText("Fixed Scope")).toBeDefined();
    });

    expect(screen.getByText("US override")).toBeDefined();
    expect(screen.queryByText("EU override")).toBeNull();
  });

  it("creates a scoped override from the structured form", async () => {
    render(
      <SuperpositionProvider
        config={{
          ...testConfig,
          scope: { context: { region: "us-east-1" } },
        }}
      >
        <AlertProvider>
          <OverrideManager />
        </AlertProvider>
      </SuperpositionProvider>,
    );

    fireEvent.click(await screen.findByText("Create override"));

    fireEvent.change(screen.getByLabelText("Config Key"), {
      target: { value: "app.title" },
    });
    fireEvent.change(screen.getByLabelText("app.title"), {
      target: { value: "Scoped App" },
    });

    fireEvent.change(screen.getByLabelText("Reason for Change*"), {
      target: { value: "test create" },
    });

    fireEvent.click(screen.getByText("Create"));

    await waitFor(() => {
      expect(mockFetch).toHaveBeenCalledWith(
        "https://test.com/context",
        expect.objectContaining({
          method: "PUT",
          body: JSON.stringify({
            context: { region: "us-east-1" },
            override: { "app.title": "Scoped App" },
            description: undefined,
            change_reason: "test create",
          }),
        }),
      );
    });
  });

  it("hides context editing when host scope is attached", async () => {
    render(
      <SuperpositionProvider
        config={{
          ...testConfig,
          scope: { context: { region: "us-east-1" } },
        }}
      >
        <AlertProvider>
          <OverrideManager />
        </AlertProvider>
      </SuperpositionProvider>,
    );

    fireEvent.click(await screen.findByText("Create override"));

    expect(screen.queryByLabelText("Dimension")).toBeNull();
    expect(screen.getByLabelText("Config Key")).toBeDefined();
  });

  it("hides mutating actions in read-only mode", async () => {
    render(
      <SuperpositionProvider
        config={{
          ...testConfig,
          readOnly: true,
        }}
      >
        <AlertProvider>
          <OverrideManager />
        </AlertProvider>
      </SuperpositionProvider>,
    );

    await waitFor(() => {
      expect(screen.getByText(/read-only mode/i)).toBeDefined();
    });

    expect(screen.queryByText("Create override")).toBeNull();
    expect(screen.queryAllByText("Delete")).toHaveLength(0);
  });
});
