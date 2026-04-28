import { act, waitFor } from "@testing-library/react";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import {
  defineCustomElements,
  registerSuperpositionHostAdapters,
  unregisterSuperpositionHostAdapters,
} from "../src/browser";

const mockConfigs = {
  total_pages: 1,
  total_items: 0,
  data: [],
};

describe("browser wrappers", () => {
  const mockFetch = vi.fn();

  beforeEach(() => {
    vi.stubGlobal("fetch", mockFetch);
    mockFetch.mockResolvedValue({
      ok: true,
      status: 200,
      headers: new Headers({ "content-length": "50" }),
      json: () => Promise.resolve(mockConfigs),
    });
  });

  afterEach(() => {
    document.body.innerHTML = "";
    unregisterSuperpositionHostAdapters("test-adapters");
    vi.restoreAllMocks();
  });

  it("registers and mounts the config manager custom element", async () => {
    const tags = defineCustomElements();
    const element = document.createElement(tags["config-manager"]);

    element.setAttribute("base-url", "https://test.com");
    element.setAttribute("org-id", "org");
    element.setAttribute("workspace", "ws");
    act(() => {
      document.body.appendChild(element);
    });

    await waitFor(() => {
      expect(element.shadowRoot?.textContent).toContain("Configs");
      expect(element.shadowRoot?.textContent).not.toContain("Create config");
    });

    expect(mockFetch).toHaveBeenCalled();
  });

  it("waits for config before rendering a custom element", async () => {
    const tags = defineCustomElements();
    const element = document.createElement(tags.admin);

    act(() => {
      document.body.appendChild(element);
    });

    expect(element.shadowRoot?.textContent).not.toContain("Failed to mount");

    act(() => {
      element.setAttribute(
        "config",
        JSON.stringify({
          apiBaseUrl: "https://test.com",
          orgId: "org",
          workspace: "ws",
          features: ["config"],
        }),
      );
    });

    await waitFor(() => {
      expect(element.shadowRoot?.textContent).toContain("Configs");
    });
  });

  it("uses registered host adapters for custom elements", async () => {
    const notify = vi.fn();
    registerSuperpositionHostAdapters("test-adapters", {
      auth: { mode: "custom", headers: { "x-adapter": "yes" } },
      ui: { notify },
    });

    const tags = defineCustomElements();
    const element = document.createElement(tags["config-manager"]);

    element.setAttribute("adapter-id", "test-adapters");
    element.setAttribute(
      "config",
      JSON.stringify({
        apiBaseUrl: "https://test.com",
        orgId: "org",
        workspace: "ws",
      }),
    );

    act(() => {
      document.body.appendChild(element);
    });

    await waitFor(() => {
      expect(element.shadowRoot?.textContent).toContain("Configs");
      expect(element.shadowRoot?.textContent).not.toContain("Create config");
    });

    expect(mockFetch).toHaveBeenCalledWith(
      expect.any(String),
      expect.objectContaining({
        headers: expect.objectContaining({ "x-adapter": "yes" }),
      }),
    );
  });
});
