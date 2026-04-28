import { fireEvent, render, screen, waitFor } from "@testing-library/react";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { SuperpositionAdmin } from "../../src/pages/SuperpositionAdmin";
import { AlertProvider } from "../../src/providers/AlertProvider";
import { SuperpositionProvider } from "../../src/providers/SuperpositionProvider";

describe("SuperpositionAdmin", () => {
  const mockFetch = vi.fn();

  beforeEach(() => {
    vi.stubGlobal("fetch", mockFetch);
    mockFetch.mockResolvedValue({
      ok: true,
      status: 200,
      headers: new Headers({ "content-length": "50" }),
      json: () =>
        Promise.resolve({
          total_pages: 1,
          total_items: 0,
          data: [],
        }),
    });
  });

  afterEach(() => {
    vi.restoreAllMocks();
  });

  it("supports host-controlled external routing", async () => {
    const onNavigate = vi.fn();

    render(
      <SuperpositionProvider
        config={{
          apiBaseUrl: "/api",
          orgId: "org",
          workspace: "ws",
          features: ["config", "overrides"],
          routing: {
            mode: "external",
            currentFeature: "config",
            onNavigate,
            getFeatureHref: (feature) => `/settings/${feature}`,
          },
        }}
      >
        <AlertProvider>
          <SuperpositionAdmin />
        </AlertProvider>
      </SuperpositionProvider>,
    );

    await waitFor(() => {
      expect(mockFetch).toHaveBeenCalled();
    });

    const overridesTab = screen.getByText("Overrides");
    expect(overridesTab.getAttribute("href")).toBe("/settings/overrides");

    fireEvent.click(overridesTab);
    expect(onNavigate).toHaveBeenCalledWith("overrides");
  });
});
