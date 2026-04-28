import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { SuperpositionClient } from "../../src/api/client";
import { resolveApi } from "../../src/api/resolve";

describe("resolveApi", () => {
  const mockFetch = vi.fn();
  const client = new SuperpositionClient({
    apiBaseUrl: "https://superposition.test",
    orgId: "test-org",
    workspace: "test-ws",
  });

  beforeEach(() => {
    vi.stubGlobal("fetch", mockFetch);
    mockFetch.mockResolvedValue({
      ok: true,
      status: 200,
      headers: new Headers({ "content-length": "2" }),
      json: () => Promise.resolve({ ok: true }),
    });
  });

  afterEach(() => {
    vi.restoreAllMocks();
  });

  it("calls GET /config/resolve with dimension query params", async () => {
    const api = resolveApi(client);

    await api.resolve({ region: "ap-south-1", city: "blr" }, "DEEP");

    const [url, init] = mockFetch.mock.calls[0];
    expect(url).toBe(
      "https://superposition.test/config/resolve?dimension[region]=ap-south-1&dimension[city]=blr&merge_strategy=DEEP",
    );
    expect(init.method).toBe("GET");
    expect(init.body).toBeUndefined();
  });
});
