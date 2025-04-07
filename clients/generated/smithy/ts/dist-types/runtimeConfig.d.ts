import { NodeHttpHandler as RequestHandler } from "@smithy/node-http-handler";
import { SuperpositionClientConfig } from "./SuperpositionClient";
/**
 * @internal
 */
export declare const getRuntimeConfig: (config: SuperpositionClientConfig) => {
    runtime: string;
    defaultsMode: import("@smithy/types").Provider<import("@smithy/smithy-client").ResolvedDefaultsMode>;
    bodyLengthChecker: import("@smithy/types").BodyLengthCalculator;
    defaultUserAgentProvider: (config?: import("@aws-sdk/util-user-agent-node").PreviouslyResolved | undefined) => Promise<import("@smithy/types").UserAgent>;
    maxAttempts: number | import("@smithy/types").Provider<number>;
    requestHandler: RequestHandler | import("@smithy/protocol-http").HttpHandler<any>;
    retryMode: string | import("@smithy/types").Provider<string>;
    sha256: import("@smithy/types").HashConstructor;
    streamCollector: import("@smithy/types").StreamCollector;
    userAgentAppId: string | import("@smithy/types").Provider<string | undefined>;
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
