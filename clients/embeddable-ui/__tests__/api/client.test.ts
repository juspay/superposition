import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { SuperpositionApiError, SuperpositionClient } from "../../src/api/client";

describe("SuperpositionClient", () => {
  let client: SuperpositionClient;
  const mockFetch = vi.fn();

  beforeEach(() => {
    client = new SuperpositionClient({
      apiBaseUrl: "https://superposition.test",
      orgId: "test-org",
      workspace: "test-ws",
      auth: {
        mode: "bearer",
        token: "Bearer test-token",
        headers: { "x-custom": "val" },
      },
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
      Accept: "application/json",
      "x-org-id": "test-org",
      "x-workspace": "test-ws",
      Authorization: "Bearer test-token",
      "x-custom": "val",
    });
  });

  it("adds the Bearer prefix when the token is raw", async () => {
    const rawTokenClient = new SuperpositionClient({
      apiBaseUrl: "https://superposition.test",
      orgId: "test-org",
      workspace: "test-ws",
      auth: { mode: "bearer", token: "raw-token" },
    });

    mockFetch.mockResolvedValue({
      ok: true,
      status: 200,
      headers: new Headers({ "content-length": "2" }),
      json: () => Promise.resolve({}),
    });

    await rawTokenClient.get("/test");

    const [, init] = mockFetch.mock.calls[0];
    expect(init.headers).toMatchObject({
      Authorization: "Bearer raw-token",
    });
  });

  it("supports the embeddable API contract and request hooks", async () => {
    const interceptRequest = vi.fn((context) => ({
      ...context,
      init: {
        ...context.init,
        headers: {
          ...(context.init.headers as Record<string, string>),
          "x-hooked": "yes",
        },
      },
    }));

    const embeddableClient = new SuperpositionClient({
      apiBaseUrl: "/proxy",
      apiBasePath: "/superposition",
      workspaceHeaderName: "x-tenant",
      orgId: "test-org",
      workspace: "tenant-a",
      auth: { mode: "custom", headers: { "x-auth": "ok" } },
      network: { interceptRequest },
    });

    mockFetch.mockResolvedValue({
      ok: true,
      status: 200,
      headers: new Headers({ "content-length": "2" }),
      json: () => Promise.resolve({}),
    });

    await embeddableClient.get("/test");

    const [url, init] = mockFetch.mock.calls[0];
    expect(url).toBe("/proxy/superposition/test");
    expect(init.headers).toMatchObject({
      "x-org-id": "test-org",
      "x-tenant": "tenant-a",
      "x-auth": "ok",
      "x-hooked": "yes",
    });
    expect(interceptRequest).toHaveBeenCalledOnce();
  });

  it("calls unauthorized and api error hooks on failed responses", async () => {
    const onUnauthorized = vi.fn();
    const onApiError = vi.fn();
    const embeddableClient = new SuperpositionClient({
      apiBaseUrl: "https://superposition.test",
      orgId: "test-org",
      workspace: "test-ws",
      auth: { mode: "cookie" },
      network: { onUnauthorized, onApiError },
    });

    mockFetch.mockResolvedValue({
      ok: false,
      status: 401,
      text: () => Promise.resolve("Unauthorized"),
    });

    await expect(embeddableClient.get("/missing")).rejects.toThrow(SuperpositionApiError);
    expect(onUnauthorized).toHaveBeenCalledOnce();
    expect(onApiError).toHaveBeenCalled();
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

  it("serializes nested query params using bracket notation", async () => {
    mockFetch.mockResolvedValue({
      ok: true,
      status: 200,
      headers: new Headers({ "content-length": "2" }),
      json: () => Promise.resolve([]),
    });

    await client.get("/resolve", {
      dimension: { region: "ap-south-1", enabled: true },
      merge_strategy: "DEEP",
    });

    const url = mockFetch.mock.calls[0][0];
    expect(url).toContain("dimension[region]=ap-south-1");
    expect(url).toContain("dimension[enabled]=true");
    expect(url).toContain("merge_strategy=DEEP");
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
    expect(init.credentials).toBe("include");
  });

  it("does not send JSON content-type for bodyless requests", async () => {
    mockFetch.mockResolvedValue({
      ok: true,
      status: 200,
      headers: new Headers({ "content-length": "2" }),
      json: () => Promise.resolve({}),
    });

    await client.get("/items");

    const init = mockFetch.mock.calls[0][1];
    expect(init.headers).not.toHaveProperty("Content-Type");
    expect(init.headers).toMatchObject({
      Accept: "application/json",
      "x-org-id": "test-org",
      "x-workspace": "test-ws",
    });
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
      apiBaseUrl: "https://example.com///",
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
