// smithy-typescript generated code
import { SuperpositionClientResolvedConfig } from "../SuperpositionClient";
import {
  doesIdentityRequireRefresh,
  isIdentityExpired,
  memoizeIdentityProvider,
} from "@smithy/core";
import {
  HandlerExecutionContext,
  HttpAuthOption,
  HttpAuthScheme,
  HttpAuthSchemeParameters,
  HttpAuthSchemeParametersProvider,
  HttpAuthSchemeProvider,
  TokenIdentity,
  TokenIdentityProvider,
} from "@smithy/types";
import { getSmithyContext } from "@smithy/util-middleware";

/**
 * @internal
 */
export interface SuperpositionHttpAuthSchemeParameters extends HttpAuthSchemeParameters {
}

/**
 * @internal
 */
export interface SuperpositionHttpAuthSchemeParametersProvider extends HttpAuthSchemeParametersProvider<SuperpositionClientResolvedConfig, HandlerExecutionContext, SuperpositionHttpAuthSchemeParameters, object> {}

/**
 * @internal
 */
export const defaultSuperpositionHttpAuthSchemeParametersProvider = async (config: SuperpositionClientResolvedConfig, context: HandlerExecutionContext, input: object): Promise<SuperpositionHttpAuthSchemeParameters> => {
  return {
    operation: getSmithyContext(context).operation as string,
  };
};

function createSmithyApiHttpBearerAuthHttpAuthOption(authParameters: SuperpositionHttpAuthSchemeParameters): HttpAuthOption {
  return {
    schemeId: "smithy.api#httpBearerAuth",
  };
};

/**
 * @internal
 */
export interface SuperpositionHttpAuthSchemeProvider extends HttpAuthSchemeProvider<SuperpositionHttpAuthSchemeParameters> {}

/**
 * @internal
 */
export const defaultSuperpositionHttpAuthSchemeProvider: SuperpositionHttpAuthSchemeProvider = (authParameters) => {
  const options: HttpAuthOption[] = [];
  switch (authParameters.operation) {
    default: {
      options.push(createSmithyApiHttpBearerAuthHttpAuthOption(authParameters));
    };
  };
  return options;
};

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
export const resolveHttpAuthSchemeConfig = <T>(config: T & HttpAuthSchemeInputConfig): T & HttpAuthSchemeResolvedConfig => {
  const token = memoizeIdentityProvider(config.token, isIdentityExpired, doesIdentityRequireRefresh);
  return {
    ...config,
    token,
  } as T & HttpAuthSchemeResolvedConfig;
};
