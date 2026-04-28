import type {
  NormalizedSuperpositionConfig,
  SuperpositionEmbeddableConfig,
} from "../types";
import { normalizeSuperpositionConfig } from "../utils/normalize-config";

export type ClientConfig = SuperpositionEmbeddableConfig;

export class SuperpositionClient {
  private config: NormalizedSuperpositionConfig;

  constructor(config: ClientConfig) {
    this.config = normalizeSuperpositionConfig(config);
  }

  get host(): string {
    return this.config.transport.baseUrl;
  }

  get orgId(): string {
    return this.config.orgId;
  }

  get workspace(): string {
    return this.config.workspace;
  }

  private joinUrl(...parts: string[]): string {
    const [first = "", ...rest] = parts;
    const base = first.replace(/\/+$/, "");

    if (rest.length === 0) {
      return base;
    }

    const suffix = rest
      .filter(Boolean)
      .map((part, index) => {
        if (index === rest.length - 1 && part === "/") {
          return "";
        }

        return part.replace(/^\/+/, "").replace(/\/+$/, "");
      })
      .filter(Boolean)
      .join("/");

    if (!base) {
      return suffix ? `/${suffix}` : "";
    }

    return suffix ? `${base}/${suffix}` : base;
  }

  private buildHeaders(
    extra?: Record<string, string>,
    hasBody = false,
  ): Record<string, string> {
    const headers: Record<string, string> = {
      Accept: "application/json",
      "x-org-id": this.config.orgId,
      [this.config.transport.workspaceHeaderName ?? "x-workspace"]: this.config.workspace,
      ...this.config.auth?.headers,
      ...extra,
    };
    if (hasBody) {
      headers["Content-Type"] = "application/json";
    }
    if (this.config.auth?.mode === "bearer" && this.config.auth.token) {
      headers["Authorization"] = this.config.auth.token.startsWith("Bearer ")
        ? this.config.auth.token
        : `Bearer ${this.config.auth.token}`;
    }
    return headers;
  }

  private encodeQueryKey(key: string): string {
    return encodeURIComponent(key).replace(/%5B/g, "[").replace(/%5D/g, "]");
  }

  private appendQueryParts(parts: string[], key: string, value: unknown): void {
    if (value === undefined || value === null) return;

    if (Array.isArray(value)) {
      if (value.length === 0) return;
      parts.push(
        `${this.encodeQueryKey(key)}=${value.map((item) => encodeURIComponent(String(item))).join(",")}`,
      );
      return;
    }

    if (typeof value === "object") {
      for (const [nestedKey, nestedValue] of Object.entries(value)) {
        this.appendQueryParts(parts, `${key}[${nestedKey}]`, nestedValue);
      }
      return;
    }

    parts.push(`${this.encodeQueryKey(key)}=${encodeURIComponent(String(value))}`);
  }

  private buildQueryString(params: Record<string, unknown>): string {
    const parts: string[] = [];
    for (const [key, value] of Object.entries(params)) {
      this.appendQueryParts(parts, key, value);
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
    const url = `${this.joinUrl(
      this.config.transport.baseUrl,
      this.config.transport.apiBasePath ?? "",
      path,
    )}${qs}`;
    const hasBody = options?.body !== undefined;

    const requestContext = this.config.network?.interceptRequest
      ? await this.config.network.interceptRequest({
          url,
          init: {
            method,
            headers: this.buildHeaders(options?.headers, hasBody),
            credentials: this.config.transport.credentials ?? "include",
            body: hasBody ? JSON.stringify(options.body) : undefined,
          },
        })
      : {
          url,
          init: {
            method,
            headers: this.buildHeaders(options?.headers, hasBody),
            credentials: this.config.transport.credentials ?? "include",
            body: hasBody ? JSON.stringify(options.body) : undefined,
          },
        };

    try {
      const rawResponse = await fetch(requestContext.url, requestContext.init);
      const response = this.config.network?.interceptResponse
        ? await this.config.network.interceptResponse({
            request: requestContext,
            response: rawResponse,
          })
        : rawResponse;

      if (!response.ok) {
        if (response.status === 401) {
          this.config.network?.onUnauthorized?.(response);
        }

        if (response.status === 403) {
          this.config.network?.onForbidden?.(response);
        }

        const errorBody = await response.text().catch(() => "Unknown error");
        const error = new SuperpositionApiError(
          response.status,
          errorBody,
          requestContext.url,
        );
        throw error;
      }

      if (response.status === 204 || response.headers.get("content-length") === "0") {
        return undefined as T;
      }

      return response.json();
    } catch (error) {
      this.config.network?.onApiError?.(error);
      throw error;
    }
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
