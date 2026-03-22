import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import { SuperpositionClient, SuperpositionApiError } from "../../src/api/client";

describe("SuperpositionClient", () => {
  let client: SuperpositionClient;
  const mockFetch = vi.fn();

  beforeEach(() => {
    client = new SuperpositionClient({
      host: "https://superposition.test",
      orgId: "test-org",
      workspace: "test-ws",
      auth: { token: "Bearer test-token", headers: { "x-custom": "val" } },
    });
    vi.stubGlobal("fetch", mockFetch);
  });

  afterEach(() => {
    vi.restoreAllMocks();
  });

  it("sends correct headers", async () => {
    mockFetch.mockResolvedValue({
      ok: true,
      status: 200,
      headers: new Headers({ "content-length": "2" }),
      json: () => Promise.resolve({}),
    });

    await client.get("/test");

    expect(mockFetch).toHaveBeenCalledOnce();
    const [url, init] = mockFetch.mock.calls[0];
    expect(url).toBe("https://superposition.test/test");
    expect(init.method).toBe("GET");
    expect(init.headers).toMatchObject({
      "Content-Type": "application/json",
      "x-org-id": "test-org",
      "x-workspace": "test-ws",
      Authorization: "Bearer test-token",
      "x-custom": "val",
    });
  });

  it("builds query strings correctly", async () => {
    mockFetch.mockResolvedValue({
      ok: true,
      status: 200,
      headers: new Headers({ "content-length": "2" }),
      json: () => Promise.resolve([]),
    });

    await client.get("/items", { page: 1, count: 10, status: ["A", "B"] });

    const url = mockFetch.mock.calls[0][0];
    expect(url).toContain("page=1");
    expect(url).toContain("count=10");
    expect(url).toContain("status=A,B");
  });

  it("skips undefined/null query params", async () => {
    mockFetch.mockResolvedValue({
      ok: true,
      status: 200,
      headers: new Headers({ "content-length": "2" }),
      json: () => Promise.resolve({}),
    });

    await client.get("/items", { page: 1, name: undefined, foo: null });

    const url = mockFetch.mock.calls[0][0];
    expect(url).toContain("page=1");
    expect(url).not.toContain("name");
    expect(url).not.toContain("foo");
  });

  it("sends POST body as JSON", async () => {
    mockFetch.mockResolvedValue({
      ok: true,
      status: 200,
      headers: new Headers({ "content-length": "10" }),
      json: () => Promise.resolve({ id: "123" }),
    });

    const body = { key: "test", value: 42 };
    await client.post("/items", body);

    const init = mockFetch.mock.calls[0][1];
    expect(init.method).toBe("POST");
    expect(init.body).toBe(JSON.stringify(body));
  });

  it("throws SuperpositionApiError on non-OK response", async () => {
    mockFetch.mockResolvedValue({
      ok: false,
      status: 404,
      text: () => Promise.resolve("Not found"),
    });

    await expect(client.get("/missing")).rejects.toThrow(SuperpositionApiError);
    await expect(client.get("/missing")).rejects.toMatchObject({
      status: 404,
      body: "Not found",
    });
  });

  it("returns undefined for 204 responses", async () => {
    mockFetch.mockResolvedValue({
      ok: true,
      status: 204,
      headers: new Headers(),
    });

    const result = await client.delete("/items/123");
    expect(result).toBeUndefined();
  });

  it("strips trailing slash from host", () => {
    const c = new SuperpositionClient({
      host: "https://example.com///",
      orgId: "o",
      workspace: "w",
    });
    expect(c.host).toBe("https://example.com");
  });

  it("skips empty arrays in query string", async () => {
    mockFetch.mockResolvedValue({
      ok: true,
      status: 200,
      headers: new Headers({ "content-length": "2" }),
      json: () => Promise.resolve([]),
    });

    await client.get("/items", { tags: [] });
    const url = mockFetch.mock.calls[0][0];
    expect(url).not.toContain("tags");
  });
});
