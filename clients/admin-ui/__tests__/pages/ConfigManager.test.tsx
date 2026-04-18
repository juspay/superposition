import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import { render, screen, waitFor } from "@testing-library/react";
import { SuperpositionProvider } from "../../src/providers/SuperpositionProvider";
import { AlertProvider } from "../../src/providers/AlertProvider";
import { ConfigManager } from "../../src/pages/ConfigManager";

const mockConfigs = {
  total_pages: 1,
  total_items: 2,
  data: [
    {
      key: "app.title",
      value: "My App",
      created_at: "2024-01-01T00:00:00Z",
      created_by: "admin",
      schema: { type: "string" },
      value_validation_function_name: null,
      last_modified_at: "2024-01-01T00:00:00Z",
      last_modified_by: "admin",
      description: "App title",
      change_reason: "init",
      value_compute_function_name: null,
    },
    {
      key: "app.version",
      value: 2,
      created_at: "2024-01-01T00:00:00Z",
      created_by: "admin",
      schema: { type: "integer" },
      value_validation_function_name: null,
      last_modified_at: "2024-01-01T00:00:00Z",
      last_modified_by: "admin",
      description: "App version",
      change_reason: "init",
      value_compute_function_name: null,
    },
  ],
};

describe("ConfigManager", () => {
  const mockFetch = vi.fn();

  beforeEach(() => {
    vi.stubGlobal("fetch", mockFetch);
    mockFetch.mockResolvedValue({
      ok: true,
      status: 200,
      headers: new Headers({ "content-length": "500" }),
      json: () => Promise.resolve(mockConfigs),
    });
  });

  afterEach(() => {
    vi.restoreAllMocks();
  });

  it("renders config list", async () => {
    render(
      <SuperpositionProvider
        config={{ host: "https://test.com", orgId: "org", workspace: "ws" }}
      >
        <AlertProvider>
          <ConfigManager />
        </AlertProvider>
      </SuperpositionProvider>,
    );

    await waitFor(() => {
      expect(screen.getByText("app.title")).toBeDefined();
      expect(screen.getByText("app.version")).toBeDefined();
    });
  });

  it("shows create button", async () => {
    render(
      <SuperpositionProvider
        config={{ host: "https://test.com", orgId: "org", workspace: "ws" }}
      >
        <AlertProvider>
          <ConfigManager />
        </AlertProvider>
      </SuperpositionProvider>,
    );

    await waitFor(() => {
      expect(screen.getByText("+ Create Config")).toBeDefined();
    });
  });

  it("shows heading", () => {
    render(
      <SuperpositionProvider
        config={{ host: "https://test.com", orgId: "org", workspace: "ws" }}
      >
        <AlertProvider>
          <ConfigManager />
        </AlertProvider>
      </SuperpositionProvider>,
    );

    expect(screen.getByText("Default Configs")).toBeDefined();
  });
});
