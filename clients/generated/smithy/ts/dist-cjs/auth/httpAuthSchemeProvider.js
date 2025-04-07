"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.resolveHttpAuthSchemeConfig = exports.defaultSuperpositionHttpAuthSchemeProvider = exports.defaultSuperpositionHttpAuthSchemeParametersProvider = void 0;
const core_1 = require("@smithy/core");
const util_middleware_1 = require("@smithy/util-middleware");
const defaultSuperpositionHttpAuthSchemeParametersProvider = async (config, context, input) => {
    return {
        operation: (0, util_middleware_1.getSmithyContext)(context).operation,
    };
};
exports.defaultSuperpositionHttpAuthSchemeParametersProvider = defaultSuperpositionHttpAuthSchemeParametersProvider;
function createSmithyApiHttpBearerAuthHttpAuthOption(authParameters) {
    return {
        schemeId: "smithy.api#httpBearerAuth",
    };
}
;
const defaultSuperpositionHttpAuthSchemeProvider = (authParameters) => {
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
exports.defaultSuperpositionHttpAuthSchemeProvider = defaultSuperpositionHttpAuthSchemeProvider;
const resolveHttpAuthSchemeConfig = (config) => {
    const token = (0, core_1.memoizeIdentityProvider)(config.token, core_1.isIdentityExpired, core_1.doesIdentityRequireRefresh);
    return {
        ...config,
        token,
    };
};
exports.resolveHttpAuthSchemeConfig = resolveHttpAuthSchemeConfig;
