import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import { SuperpositionClient } from "../../src/api/client";
import { dimensionsApi } from "../../src/api/dimensions";

describe("dimensionsApi", () => {
  const mockFetch = vi.fn();
  let api: ReturnType<typeof dimensionsApi>;

  beforeEach(() => {
    vi.stubGlobal("fetch", mockFetch);
    const client = new SuperpositionClient({
      host: "https://test.com",
      orgId: "org",
      workspace: "ws",
    });
    api = dimensionsApi(client);
  });

  afterEach(() => {
    vi.restoreAllMocks();
  });

  it("lists dimensions with pagination", async () => {
    const mockData = { total_pages: 1, total_items: 1, data: [{ dimension: "region" }] };
    mockFetch.mockResolvedValue({
      ok: true,
      status: 200,
      headers: new Headers({ "content-length": "100" }),
      json: () => Promise.resolve(mockData),
    });

    const result = await api.list({ page: 1, count: 10 });
    expect(result).toEqual(mockData);

    const url = mockFetch.mock.calls[0][0];
    expect(url).toContain("/dimension");
    expect(url).toContain("page=1");
    expect(url).toContain("count=10");
  });

  it("gets a single dimension", async () => {
    const mockDim = { dimension: "region", position: 0 };
    mockFetch.mockResolvedValue({
      ok: true,
      status: 200,
      headers: new Headers({ "content-length": "50" }),
      json: () => Promise.resolve(mockDim),
    });

    const result = await api.get("region");
    expect(result).toEqual(mockDim);
    expect(mockFetch.mock.calls[0][0]).toContain("/dimension/region");
  });

  it("creates a dimension", async () => {
    const req = {
      dimension: "country",
      position: 1,
      schema: { type: "string" },
      description: "Country dim",
      change_reason: "test",
    };
    mockFetch.mockResolvedValue({
      ok: true,
      status: 200,
      headers: new Headers({ "content-length": "50" }),
      json: () => Promise.resolve({ ...req }),
    });

    await api.create(req);
    const init = mockFetch.mock.calls[0][1];
    expect(init.method).toBe("POST");
    expect(JSON.parse(init.body)).toMatchObject(req);
  });

  it("deletes a dimension", async () => {
    mockFetch.mockResolvedValue({ ok: true, status: 204, headers: new Headers() });

    await api.delete("old-dim");
    const [url, init] = mockFetch.mock.calls[0];
    expect(url).toContain("/dimension/old-dim");
    expect(init.method).toBe("DELETE");
  });
});
