import { SuperpositionClientResolvedConfig } from "../SuperpositionClient";
import { HandlerExecutionContext, HttpAuthScheme, HttpAuthSchemeParameters, HttpAuthSchemeParametersProvider, HttpAuthSchemeProvider, TokenIdentity, TokenIdentityProvider } from "@smithy/types";
/**
 * @internal
 */
export interface SuperpositionHttpAuthSchemeParameters extends HttpAuthSchemeParameters {
}
/**
 * @internal
 */
export interface SuperpositionHttpAuthSchemeParametersProvider extends HttpAuthSchemeParametersProvider<SuperpositionClientResolvedConfig, HandlerExecutionContext, SuperpositionHttpAuthSchemeParameters, object> {
}
/**
 * @internal
 */
export declare const defaultSuperpositionHttpAuthSchemeParametersProvider: (config: SuperpositionClientResolvedConfig, context: HandlerExecutionContext, input: object) => Promise<SuperpositionHttpAuthSchemeParameters>;
/**
 * @internal
 */
export interface SuperpositionHttpAuthSchemeProvider extends HttpAuthSchemeProvider<SuperpositionHttpAuthSchemeParameters> {
}
/**
 * @internal
 */
export declare const defaultSuperpositionHttpAuthSchemeProvider: SuperpositionHttpAuthSchemeProvider;
/**
 * @internal
 */
export interface HttpAuthSchemeInputConfig {
    /**
     * Configuration of HttpAuthSchemes for a client which provides default identity providers and signers per auth scheme.
     * @internal
     */
    httpAuthSchemes?: HttpAuthScheme[];
    /**
     * Configuration of an HttpAuthSchemeProvider for a client which resolves which HttpAuthScheme to use.
     * @internal
     */
    httpAuthSchemeProvider?: SuperpositionHttpAuthSchemeProvider;
    /**
     * The token used to authenticate requests.
     */
    token?: TokenIdentity | TokenIdentityProvider;
}
/**
 * @internal
 */
export interface HttpAuthSchemeResolvedConfig {
    /**
     * Configuration of HttpAuthSchemes for a client which provides default identity providers and signers per auth scheme.
     * @internal
     */
    readonly httpAuthSchemes: HttpAuthScheme[];
    /**
     * Configuration of an HttpAuthSchemeProvider for a client which resolves which HttpAuthScheme to use.
     * @internal
     */
    readonly httpAuthSchemeProvider: SuperpositionHttpAuthSchemeProvider;
    /**
     * The token used to authenticate requests.
     */
    readonly token?: TokenIdentityProvider;
}
/**
 * @internal
 */
export declare const resolveHttpAuthSchemeConfig: <T>(config: T & HttpAuthSchemeInputConfig) => T & HttpAuthSchemeResolvedConfig;
