import { defaultSuperpositionHttpAuthSchemeProvider } from "./auth/httpAuthSchemeProvider";
import { HttpBearerAuthSigner } from "@smithy/core";
import { NoOpLogger } from "@smithy/smithy-client";
import { parseUrl } from "@smithy/url-parser";
import { fromBase64, toBase64, } from "@smithy/util-base64";
import { fromUtf8, toUtf8, } from "@smithy/util-utf8";
export const getRuntimeConfig = (config) => {
    return {
        apiVersion: "2025-03-05",
        base64Decoder: config?.base64Decoder ?? fromBase64,
        base64Encoder: config?.base64Encoder ?? toBase64,
        disableHostPrefix: config?.disableHostPrefix ?? false,
        extensions: config?.extensions ?? [],
        httpAuthSchemeProvider: config?.httpAuthSchemeProvider ?? defaultSuperpositionHttpAuthSchemeProvider,
        httpAuthSchemes: config?.httpAuthSchemes ?? [{
                schemeId: "smithy.api#httpBearerAuth",
                identityProvider: (ipc) => ipc.getIdentityProvider("smithy.api#httpBearerAuth"),
                signer: new HttpBearerAuthSigner(),
            }],
        logger: config?.logger ?? new NoOpLogger(),
        urlParser: config?.urlParser ?? parseUrl,
        utf8Decoder: config?.utf8Decoder ?? fromUtf8,
        utf8Encoder: config?.utf8Encoder ?? toUtf8,
    };
};
