import { doesIdentityRequireRefresh, isIdentityExpired, memoizeIdentityProvider, } from "@smithy/core";
import { getSmithyContext } from "@smithy/util-middleware";
export const defaultSuperpositionHttpAuthSchemeParametersProvider = async (config, context, input) => {
    return {
        operation: getSmithyContext(context).operation,
    };
};
function createSmithyApiHttpBearerAuthHttpAuthOption(authParameters) {
    return {
        schemeId: "smithy.api#httpBearerAuth",
    };
}
;
export const defaultSuperpositionHttpAuthSchemeProvider = (authParameters) => {
    const options = [];
    switch (authParameters.operation) {
        default:
            {
                options.push(createSmithyApiHttpBearerAuthHttpAuthOption(authParameters));
            }
            ;
    }
    ;
    return options;
};
export const resolveHttpAuthSchemeConfig = (config) => {
    const token = memoizeIdentityProvider(config.token, isIdentityExpired, doesIdentityRequireRefresh);
    return {
        ...config,
        token,
    };
};
