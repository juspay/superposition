import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { SuperpositionClient } from "../../src/api/client";
import { defaultConfigsApi } from "../../src/api/default-configs";

describe("defaultConfigsApi", () => {
  const mockFetch = vi.fn();
  let api: ReturnType<typeof defaultConfigsApi>;

  beforeEach(() => {
    vi.stubGlobal("fetch", mockFetch);
    api = defaultConfigsApi(
      new SuperpositionClient({
        apiBaseUrl: "https://superposition.test",
        orgId: "test-org",
        workspace: "test-ws",
        auth: { mode: "bearer", token: "Bearer test-token" },
      }),
    );
  });

  afterEach(() => {
    vi.restoreAllMocks();
  });

  it("returns an empty page when backend reports no default configs", async () => {
    mockFetch.mockResolvedValue({
      ok: false,
      status: 404,
      text: () =>
        Promise.resolve(
          JSON.stringify({
            message: "No records found. Please refine or correct your search parameters",
          }),
        ),
    });

    await expect(api.list({ page: 1, count: 20 })).resolves.toEqual({
      total_pages: 0,
      total_items: 0,
      data: [],
    });
  });

  it("sends workspace, org, auth headers, and payload on create", async () => {
    const payload = {
      key: "checkout.title",
      value: "Fast Checkout",
      schema: { type: "string" },
      description: "Checkout page title",
      change_reason: "initial setup",
    };

    mockFetch.mockResolvedValue({
      ok: true,
      status: 200,
      headers: new Headers({ "content-length": "200" }),
      json: () =>
        Promise.resolve({
          ...payload,
          created_at: "2026-04-25T00:00:00Z",
          created_by: "admin",
          last_modified_at: "2026-04-25T00:00:00Z",
          last_modified_by: "admin",
          value_validation_function_name: null,
          value_compute_function_name: null,
        }),
    });

    await api.create(payload);

    expect(mockFetch).toHaveBeenCalledOnce();
    const [url, init] = mockFetch.mock.calls[0];
    expect(url).toBe("https://superposition.test/default-config");
    expect(init.method).toBe("POST");
    expect(init.headers).toMatchObject({
      Accept: "application/json",
      "Content-Type": "application/json",
      "x-org-id": "test-org",
      "x-workspace": "test-ws",
      Authorization: "Bearer test-token",
    });
    expect(init.body).toBe(JSON.stringify(payload));
  });
});
