"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.resolveHttpAuthRuntimeConfig = exports.getHttpAuthExtensionConfiguration = void 0;
const getHttpAuthExtensionConfiguration = (runtimeConfig) => {
    let _httpAuthSchemes = runtimeConfig.httpAuthSchemes;
    let _httpAuthSchemeProvider = runtimeConfig.httpAuthSchemeProvider;
    let _token = runtimeConfig.token;
    return {
        setHttpAuthScheme(httpAuthScheme) {
            const index = _httpAuthSchemes.findIndex(scheme => scheme.schemeId === httpAuthScheme.schemeId);
            if (index === -1) {
                _httpAuthSchemes.push(httpAuthScheme);
            }
            else {
                _httpAuthSchemes.splice(index, 1, httpAuthScheme);
            }
        },
        httpAuthSchemes() {
            return _httpAuthSchemes;
        },
        setHttpAuthSchemeProvider(httpAuthSchemeProvider) {
            _httpAuthSchemeProvider = httpAuthSchemeProvider;
        },
        httpAuthSchemeProvider() {
            return _httpAuthSchemeProvider;
        },
        setToken(token) {
            _token = token;
        },
        token() {
            return _token;
        },
    };
};
exports.getHttpAuthExtensionConfiguration = getHttpAuthExtensionConfiguration;
const resolveHttpAuthRuntimeConfig = (config) => {
    return {
        httpAuthSchemes: config.httpAuthSchemes(),
        httpAuthSchemeProvider: config.httpAuthSchemeProvider(),
        token: config.token(),
    };
};
exports.resolveHttpAuthRuntimeConfig = resolveHttpAuthRuntimeConfig;
