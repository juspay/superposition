import { FetchHttpHandler as RequestHandler } from "@smithy/fetch-http-handler";
import { SuperpositionClientConfig } from "./SuperpositionClient";
/**
 * @internal
 */
export declare const getRuntimeConfig: (config: SuperpositionClientConfig) => {
    runtime: string;
    defaultsMode: import("@smithy/types").Provider<import("@smithy/smithy-client").ResolvedDefaultsMode>;
    bodyLengthChecker: import("@smithy/types").BodyLengthCalculator;
    defaultUserAgentProvider: (config?: import("@aws-sdk/util-user-agent-browser").PreviouslyResolved | undefined) => Promise<import("@smithy/types").UserAgent>;
    maxAttempts: number | import("@smithy/types").Provider<number>;
    requestHandler: import("@smithy/protocol-http").HttpHandler<any> | RequestHandler;
    retryMode: string | import("@smithy/types").Provider<string>;
    sha256: import("@smithy/types").HashConstructor;
    streamCollector: import("@smithy/types").StreamCollector;
    apiVersion: string;
    cacheMiddleware?: boolean | undefined;
    urlParser: import("@smithy/types").UrlParser;
    base64Decoder: import("@smithy/types").Decoder;
    base64Encoder: (_input: string | Uint8Array) => string;
    utf8Decoder: import("@smithy/types").Decoder;
    utf8Encoder: (input: string | Uint8Array) => string;
    disableHostPrefix: boolean;
    profile?: string | undefined;
    logger: import("@smithy/types").Logger;
    extensions: import("./runtimeExtensions").RuntimeExtension[];
    customUserAgent?: string | import("@smithy/types").UserAgent | undefined;
    userAgentAppId?: string | import("@smithy/types").Provider<string | undefined> | undefined;
    endpoint: string | import("@smithy/types").Endpoint | import("@smithy/types").Provider<import("@smithy/types").Endpoint>;
    tls?: boolean | undefined;
    useDualstackEndpoint?: boolean | import("@smithy/types").Provider<boolean> | undefined;
    retryStrategy?: import("@smithy/types").RetryStrategy | import("@smithy/types").RetryStrategyV2 | undefined;
    httpAuthSchemes: {
        schemeId: string;
        identityProvider: (ipc: import("@smithy/types").IdentityProviderConfig) => import("@smithy/types").IdentityProvider<import("@smithy/types").Identity> | undefined;
        signer: import("@smithy/core").HttpBearerAuthSigner;
    }[];
    httpAuthSchemeProvider: import("./auth/httpAuthSchemeProvider").SuperpositionHttpAuthSchemeProvider;
    token?: import("@smithy/types").TokenIdentity | import("@smithy/types").TokenIdentityProvider | undefined;
};
