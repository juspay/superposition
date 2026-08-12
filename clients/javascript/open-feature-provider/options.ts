/**
 * Provider configuration: authentication, connection details, and refresh strategies.
 *
 * Mirrors the Rust/Python/Java options (`AuthMethod`, `SuperpositionOptions`, `RefreshStrategy`),
 * expressed with TypeScript discriminated unions. All durations are milliseconds — like the Java
 * client (a fresh client, never published with seconds), the strategy types carry only
 * `...Milliseconds` fields and no deprecated seconds fields.
 */

import { Buffer } from "buffer";
import { HttpRequest } from "@smithy/protocol-http";
import type { Identity } from "@smithy/types";

// ============================================================================
// Authentication
// ============================================================================

/** Bearer-token authentication. */
export interface TokenAuth {
    readonly kind: "token";
    readonly token: string;
}

/** HTTP basic authentication. */
export interface BasicAuth {
    readonly kind: "basic";
    readonly username: string;
    readonly password: string;
}

/** How to authenticate with the Superposition backend. */
export type AuthMethod = TokenAuth | BasicAuth;

// eslint-disable-next-line @typescript-eslint/no-namespace
export namespace AuthMethod {
    /** Bearer-token authentication. */
    export function token(token: string): TokenAuth {
        return { kind: "token", token };
    }

    /** HTTP basic authentication. */
    export function basic(username: string, password: string): BasicAuth {
        return { kind: "basic", username, password };
    }
}

/** Basic-auth credentials carried as the request identity. */
interface BasicAuthIdentity extends Identity {
    readonly username: string;
    readonly password: string;
}

/**
 * Basic-auth signer — `@smithy/core` ships a bearer signer but no basic one, so this supplies the
 * `Authorization: Basic <base64>` header itself. Structurally an `HttpSigner`, minus the unused
 * `signingProperties` argument.
 */
const httpBasicAuthSigner = {
    async sign(
        httpRequest: HttpRequest,
        identity: BasicAuthIdentity,
    ): Promise<HttpRequest> {
        const cloned = HttpRequest.clone(httpRequest);
        const encoded = Buffer.from(
            `${identity.username}:${identity.password}`,
        ).toString("base64");
        cloned.headers["Authorization"] = `Basic ${encoded}`;
        return cloned;
    },
};

/**
 * Build the SDK client-config fragment for the given auth method.
 *
 * One place that maps an {@link AuthMethod} to the SDK's auth scheme, mirroring the Rust client's
 * `From<&SuperpositionOptions> for Config` and Python's `auth_scheme_config` — every client-creation
 * site goes through here rather than hard-coding bearer auth.
 */
export function sdkAuthConfig(auth: AuthMethod): Record<string, any> {
    switch (auth.kind) {
        case "token":
            // Uses the SDK's default bearer scheme.
            return { token: { token: auth.token } };
        case "basic":
            // The default `httpAuthSchemes` only wires bearer, so supply a basic scheme explicitly.
            // The auth-scheme provider offers basic before bearer, so this is what gets selected.
            return {
                httpAuthSchemes: [
                    {
                        schemeId: "smithy.api#httpBasicAuth",
                        identityProvider: () => async () => ({
                            username: auth.username,
                            password: auth.password,
                        }),
                        signer: httpBasicAuthSigner,
                    },
                ],
            };
    }
}

// ============================================================================
// Connection details
// ============================================================================

/**
 * Connection details for the Superposition backend. Used directly by data sources, or wrapped in
 * provider options.
 */
export interface SuperpositionOptions {
    /** The API endpoint (e.g. `http://localhost:8080`). */
    endpoint: string;
    /** How to authenticate — a bearer token or basic credentials. */
    auth: AuthMethod;
    /** Organization ID within Superposition. */
    orgId: string;
    /** Workspace ID for the configuration set. */
    workspaceId: string;
}

/**
 * Check that every field of {@link SuperpositionOptions} carries a value.
 *
 * @throws SuperpositionError-free plain `Error` on the first blank field (validation, not a runtime
 * failure), matching the Java `validate()`.
 */
export function validateOptions(options: SuperpositionOptions): void {
    const blank = (s: string | undefined): boolean =>
        !s || s.trim().length === 0;
    if (blank(options.endpoint)) throw new Error("endpoint is required");
    switch (options.auth.kind) {
        case "token":
            if (blank(options.auth.token)) throw new Error("token is required");
            break;
        case "basic":
            if (blank(options.auth.username))
                throw new Error("username is required");
            if (blank(options.auth.password))
                throw new Error("password is required");
            break;
    }
    if (blank(options.orgId)) throw new Error("orgId is required");
    if (blank(options.workspaceId)) throw new Error("workspaceId is required");
}

// ============================================================================
// Refresh strategies (all durations in milliseconds)
// ============================================================================

/** Fetch periodically at a fixed interval, on a background task started at initialization. */
export interface PollingStrategy {
    readonly kind: "polling";
    /** How long a single refresh may take before it is abandoned. */
    readonly timeoutMilliseconds: number;
    /** How often to poll. */
    readonly intervalMilliseconds: number;
}

/**
 * Fetch lazily, when the cached data is older than its TTL. Keeps backend load down at the cost of a
 * bounded amount of staleness; if a refresh fails and {@link useStaleOnError} is set, the last known
 * good data is served rather than failing the call.
 */
export interface OnDemandStrategy {
    readonly kind: "onDemand";
    readonly timeoutMilliseconds: number;
    /** How long cached data stays fresh. */
    readonly ttlMilliseconds: number;
    /** Whether to serve stale data when a refresh fails. */
    readonly useStaleOnError: boolean;
}

/**
 * Refresh when the underlying source signals a change. Only usable with a data source that supports
 * watching; a provider configured this way against one that does not fails to initialize rather than
 * silently never refreshing.
 */
export interface WatchStrategy {
    readonly kind: "watch";
    /** How long to coalesce a burst of rapid changes. */
    readonly debounceMs: number;
}

/**
 * Never refresh on its own; the caller drives it by invoking `refresh()`. Carries no configuration
 * (matching Rust's unit `Manual` variant) and runs unbounded — only Polling/OnDemand have a timeout.
 */
export interface ManualStrategy {
    readonly kind: "manual";
}

/** How a provider keeps its cached configuration current. */
export type RefreshStrategy =
    | PollingStrategy
    | OnDemandStrategy
    | WatchStrategy
    | ManualStrategy;

// eslint-disable-next-line @typescript-eslint/no-namespace
export namespace RefreshStrategy {
    export function polling(
        intervalMilliseconds: number,
        timeoutMilliseconds = 30_000,
    ): PollingStrategy {
        return { kind: "polling", intervalMilliseconds, timeoutMilliseconds };
    }

    export function onDemand(
        ttlMilliseconds: number,
        useStaleOnError = true,
        timeoutMilliseconds = 30_000,
    ): OnDemandStrategy {
        return {
            kind: "onDemand",
            ttlMilliseconds,
            useStaleOnError,
            timeoutMilliseconds,
        };
    }

    export function watch(debounceMs = 500): WatchStrategy {
        return { kind: "watch", debounceMs };
    }

    export function manual(): ManualStrategy {
        return { kind: "manual" };
    }
}

export function defaultPollingStrategy(): PollingStrategy {
    return RefreshStrategy.polling(60_000, 30_000);
}

export function defaultOnDemandStrategy(): OnDemandStrategy {
    return RefreshStrategy.onDemand(300_000, true, 30_000);
}

export function defaultWatchStrategy(): WatchStrategy {
    return RefreshStrategy.watch(500);
}
