import { fireEvent, render, screen, waitFor } from "@testing-library/react";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { ConfigManager } from "../../src/pages/ConfigManager";
import { AlertProvider } from "../../src/providers/AlertProvider";
import { SuperpositionProvider } from "../../src/providers/SuperpositionProvider";

const testConfig = {
  apiBaseUrl: "https://test.com",
  orgId: "org",
  workspace: "ws",
};

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
      <SuperpositionProvider config={testConfig}>
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
      <SuperpositionProvider config={testConfig}>
        <AlertProvider>
          <ConfigManager />
        </AlertProvider>
      </SuperpositionProvider>,
    );

    await waitFor(() => {
      expect(screen.getByText("Create config")).toBeDefined();
    });
  });

  it("shows heading", async () => {
    render(
      <SuperpositionProvider config={testConfig}>
        <AlertProvider>
          <ConfigManager />
        </AlertProvider>
      </SuperpositionProvider>,
    );

    await waitFor(() => {
      expect(mockFetch).toHaveBeenCalled();
      expect(screen.getByRole("heading", { name: "Configs" })).toBeDefined();
    });
  });

  it("shows inline validation for invalid JSON before create", async () => {
    render(
      <SuperpositionProvider config={testConfig}>
        <AlertProvider>
          <ConfigManager />
        </AlertProvider>
      </SuperpositionProvider>,
    );

    fireEvent.click(await screen.findByText("Create config"));
    fireEvent.change(screen.getByLabelText("Key*"), {
      target: { value: "key1" },
    });
    fireEvent.change(screen.getByLabelText("Value*"), {
      target: { value: "value1" },
    });
    fireEvent.change(screen.getByLabelText("Change Reason*"), {
      target: { value: "test reason" },
    });

    expect(screen.getByText(/Plain text is stored as a string/i)).toBeDefined();
    expect(screen.getByText("Create")).not.toBeDisabled();
  });

  it("creates a config when plain text value is entered", async () => {
    mockFetch.mockImplementation((_url: string, init?: RequestInit) => {
      if (init?.method === "POST") {
        return Promise.resolve({
          ok: true,
          status: 200,
          headers: new Headers({ "content-length": "250" }),
          json: () =>
            Promise.resolve({
              key: "key1",
              value: "value1",
              schema: { type: "string" },
              created_at: "2024-01-01T00:00:00Z",
              created_by: "admin",
              value_validation_function_name: null,
              last_modified_at: "2024-01-01T00:00:00Z",
              last_modified_by: "admin",
              description: "desc",
              change_reason: "test reason",
              value_compute_function_name: null,
            }),
        });
      }

      return Promise.resolve({
        ok: true,
        status: 200,
        headers: new Headers({ "content-length": "500" }),
        json: () => Promise.resolve(mockConfigs),
      });
    });

    render(
      <SuperpositionProvider config={testConfig}>
        <AlertProvider>
          <ConfigManager />
        </AlertProvider>
      </SuperpositionProvider>,
    );

    fireEvent.click(await screen.findByText("Create config"));
    fireEvent.change(screen.getByLabelText("Key*"), {
      target: { value: "key1" },
    });
    fireEvent.change(screen.getByLabelText("Value*"), {
      target: { value: "value1" },
    });
    fireEvent.change(screen.getByLabelText("Description"), {
      target: { value: "desc" },
    });
    fireEvent.change(screen.getByLabelText("Change Reason*"), {
      target: { value: "test reason" },
    });

    fireEvent.click(screen.getByText("Create"));

    await waitFor(() => {
      expect(mockFetch).toHaveBeenCalledWith(
        "https://test.com/default-config",
        expect.objectContaining({
          method: "POST",
          body: JSON.stringify({
            key: "key1",
            value: "value1",
            schema: { type: "string" },
            description: "desc",
            change_reason: "test reason",
          }),
        }),
      );
    });
  });

  it("applies configured prefix filters and displays resolved scoped values", async () => {
    mockFetch.mockImplementation((url: string) => {
      if (url.includes("/config/resolve")) {
        return Promise.resolve({
          ok: true,
          status: 200,
          headers: new Headers({ "content-length": "100" }),
          json: () => Promise.resolve({ "app.title": "Resolved US App" }),
        });
      }

      return Promise.resolve({
        ok: true,
        status: 200,
        headers: new Headers({ "content-length": "500" }),
        json: () => Promise.resolve(mockConfigs),
      });
    });

    render(
      <SuperpositionProvider
        config={{
          ...testConfig,
          scope: { context: { region: "us-east-1" } },
          filters: { defaultConfigPrefix: "app." },
        }}
      >
        <AlertProvider>
          <ConfigManager />
        </AlertProvider>
      </SuperpositionProvider>,
    );

    await waitFor(() => {
      expect(screen.getByText(/Resolved US App/)).toBeDefined();
    });

    expect(screen.getByText("app.title")).toBeDefined();
    expect(screen.getByText("app.version")).toBeDefined();
    expect(mockFetch).toHaveBeenCalledWith(
      "https://test.com/default-config?page=1&count=20&prefix=app.",
      expect.objectContaining({ method: "GET" }),
    );
    expect(mockFetch).toHaveBeenCalledWith(
      "https://test.com/config/resolve?dimension[region]=us-east-1",
      expect.objectContaining({ method: "GET" }),
    );
  });
});
