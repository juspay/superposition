import { SuperpositionHttpAuthSchemeProvider } from "./httpAuthSchemeProvider";
import { HttpAuthScheme, TokenIdentity, TokenIdentityProvider } from "@smithy/types";
/**
 * @internal
 */
export interface HttpAuthExtensionConfiguration {
    setHttpAuthScheme(httpAuthScheme: HttpAuthScheme): void;
    httpAuthSchemes(): HttpAuthScheme[];
    setHttpAuthSchemeProvider(httpAuthSchemeProvider: SuperpositionHttpAuthSchemeProvider): void;
    httpAuthSchemeProvider(): SuperpositionHttpAuthSchemeProvider;
    setToken(token: TokenIdentity | TokenIdentityProvider): void;
    token(): TokenIdentity | TokenIdentityProvider | undefined;
}
/**
 * @internal
 */
export type HttpAuthRuntimeConfig = Partial<{
    httpAuthSchemes: HttpAuthScheme[];
    httpAuthSchemeProvider: SuperpositionHttpAuthSchemeProvider;
    token: TokenIdentity | TokenIdentityProvider;
}>;
/**
 * @internal
 */
export declare const getHttpAuthExtensionConfiguration: (runtimeConfig: HttpAuthRuntimeConfig) => HttpAuthExtensionConfiguration;
/**
 * @internal
 */
export declare const resolveHttpAuthRuntimeConfig: (config: HttpAuthExtensionConfiguration) => HttpAuthRuntimeConfig;
