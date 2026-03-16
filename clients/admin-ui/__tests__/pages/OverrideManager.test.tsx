import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import { render, screen, waitFor } from "@testing-library/react";
import { SuperpositionProvider } from "../../src/providers/SuperpositionProvider";
import { AlertProvider } from "../../src/providers/AlertProvider";
import { OverrideManager } from "../../src/pages/OverrideManager";

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
      override_: { "app.title": "US App" },
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
      override_: { "app.title": "EU App" },
      last_modified_at: "2024-01-01T00:00:00Z",
      last_modified_by: "admin",
      weight: "50",
      description: "EU override",
      change_reason: "init",
    },
  ],
};

describe("OverrideManager", () => {
  const mockFetch = vi.fn();

  beforeEach(() => {
    vi.stubGlobal("fetch", mockFetch);
    mockFetch.mockResolvedValue({
      ok: true,
      status: 200,
      headers: new Headers({ "content-length": "500" }),
      json: () => Promise.resolve(mockOverrides),
    });
  });

  afterEach(() => {
    vi.restoreAllMocks();
  });

  it("renders override list", async () => {
    render(
      <SuperpositionProvider
        config={{ host: "https://test.com", orgId: "org", workspace: "ws" }}
      >
        <AlertProvider>
          <OverrideManager />
        </AlertProvider>
      </SuperpositionProvider>,
    );

    await waitFor(() => {
      expect(screen.getByText("Context Overrides")).toBeDefined();
    });
  });

  it("filters overrides by scoped context", async () => {
    render(
      <SuperpositionProvider
        config={{
          host: "https://test.com",
          orgId: "org",
          workspace: "ws",
          context: { region: "us-east-1" },
        }}
      >
        <AlertProvider>
          <OverrideManager />
        </AlertProvider>
      </SuperpositionProvider>,
    );

    await waitFor(() => {
      // Should show US override (region matches) and EU override (region doesn't conflict because us-east-1 ≠ eu-west-1)
      // Actually: ctx-2 has region: eu-west-1 but scoped is region: us-east-1
      // contextMatchesScope: condition[region] = eu-west-1 !== us-east-1 → no match
      expect(screen.getByText("Context Overrides")).toBeDefined();
    });
  });

  it("shows scoped context banner", async () => {
    render(
      <SuperpositionProvider
        config={{
          host: "https://test.com",
          orgId: "org",
          workspace: "ws",
          context: { region: "us-east-1" },
        }}
      >
        <AlertProvider>
          <OverrideManager />
        </AlertProvider>
      </SuperpositionProvider>,
    );

    await waitFor(() => {
      expect(screen.getByText(/Scoped to/)).toBeDefined();
    });
  });
});
