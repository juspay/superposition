export interface ClientConfig {
  host: string;
  orgId: string;
  workspace: string;
  auth?: {
    token?: string;
    headers?: Record<string, string>;
  };
}

export class SuperpositionClient {
  private config: ClientConfig;

  constructor(config: ClientConfig) {
    this.config = config;
  }

  get host(): string {
    return this.config.host.replace(/\/+$/, "");
  }

  get orgId(): string {
    return this.config.orgId;
  }

  get workspace(): string {
    return this.config.workspace;
  }

  private buildHeaders(extra?: Record<string, string>): Record<string, string> {
    const headers: Record<string, string> = {
      "Content-Type": "application/json",
      "x-org-id": this.config.orgId,
      "x-workspace": this.config.workspace,
      ...this.config.auth?.headers,
      ...extra,
    };
    if (this.config.auth?.token) {
      headers["Authorization"] = this.config.auth.token;
    }
    return headers;
  }

  private buildQueryString(params: Record<string, unknown>): string {
    const parts: string[] = [];
    for (const [key, value] of Object.entries(params)) {
      if (value === undefined || value === null) continue;
      if (Array.isArray(value)) {
        if (value.length > 0) {
          parts.push(`${encodeURIComponent(key)}=${value.map(encodeURIComponent).join(",")}`);
        }
      } else {
        parts.push(`${encodeURIComponent(key)}=${encodeURIComponent(String(value))}`);
      }
    }
    return parts.length > 0 ? `?${parts.join("&")}` : "";
  }

  async request<T>(
    method: string,
    path: string,
    options?: {
      body?: unknown;
      query?: Record<string, unknown>;
      headers?: Record<string, string>;
    },
  ): Promise<T> {
    const qs = options?.query ? this.buildQueryString(options.query) : "";
    const url = `${this.host}${path}${qs}`;

    const response = await fetch(url, {
      method,
      headers: this.buildHeaders(options?.headers),
      body: options?.body ? JSON.stringify(options.body) : undefined,
    });

    if (!response.ok) {
      const errorBody = await response.text().catch(() => "Unknown error");
      throw new SuperpositionApiError(response.status, errorBody, url);
    }

    if (response.status === 204 || response.headers.get("content-length") === "0") {
      return undefined as T;
    }

    return response.json();
  }

  get<T>(path: string, query?: Record<string, unknown>): Promise<T> {
    return this.request<T>("GET", path, { query });
  }

  post<T>(path: string, body?: unknown, query?: Record<string, unknown>): Promise<T> {
    return this.request<T>("POST", path, { body, query });
  }

  patch<T>(path: string, body?: unknown): Promise<T> {
    return this.request<T>("PATCH", path, { body });
  }

  put<T>(path: string, body?: unknown): Promise<T> {
    return this.request<T>("PUT", path, { body });
  }

  delete<T>(path: string): Promise<T> {
    return this.request<T>("DELETE", path);
  }
}

export class SuperpositionApiError extends Error {
  constructor(
    public status: number,
    public body: string,
    public url: string,
  ) {
    super(`API error ${status} for ${url}: ${body}`);
    this.name = "SuperpositionApiError";
  }
}
