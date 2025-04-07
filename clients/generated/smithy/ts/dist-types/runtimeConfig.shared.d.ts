import { HttpBearerAuthSigner } from "@smithy/core";
import { IdentityProviderConfig } from "@smithy/types";
import { SuperpositionClientConfig } from "./SuperpositionClient";
/**
 * @internal
 */
export declare const getRuntimeConfig: (config: SuperpositionClientConfig) => {
    apiVersion: string;
    base64Decoder: import("@smithy/types").Decoder;
    base64Encoder: (_input: string | Uint8Array) => string;
    disableHostPrefix: boolean;
    extensions: import("./runtimeExtensions").RuntimeExtension[];
    httpAuthSchemeProvider: import("./auth/httpAuthSchemeProvider").SuperpositionHttpAuthSchemeProvider;
    httpAuthSchemes: {
        schemeId: string;
        identityProvider: (ipc: IdentityProviderConfig) => import("@smithy/types").IdentityProvider<import("@smithy/types").Identity> | undefined;
        signer: HttpBearerAuthSigner;
    }[];
    logger: import("@smithy/types").Logger;
    urlParser: import("@smithy/types").UrlParser;
    utf8Decoder: import("@smithy/types").Decoder;
    utf8Encoder: (input: string | Uint8Array) => string;
};
