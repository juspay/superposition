"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.SuperpositionProvider = void 0;
const server_sdk_1 = require("@openfeature/server-sdk");
const configuration_client_1 = require("./configuration-client");
const utils_1 = require("./utils");
const superposition_bindings_1 = require("superposition-bindings");
const TYPE_CONVERTERS = {
    boolean: utils_1.convertToBoolean,
    string: utils_1.convertToString,
    number: utils_1.convertToNumber,
    object: utils_1.convertToObject,
};
class SuperpositionProvider {
    constructor(config) {
        this.config = config;
        this.metadata = {
            name: 'SuperpositionProvider',
            slug: 'superposition-provider'
        };
        this.events = new server_sdk_1.OpenFeatureEventEmitter();
        this.hooks = [];
        this.status = server_sdk_1.ProviderStatus.NOT_READY;
        // Cache for processed contexts
        this.processedContextCache = new WeakMap();
        this.client = new configuration_client_1.ConfigurationClient({
            endpoint: config.endpoint,
            token: config.token,
            org_id: config.org_id,
            workspace_id: config.workspace_id,
        }, new superposition_bindings_1.NativeResolver(), {
            fallbackConfig: config.fallbackConfig,
            evaluationCache: config.evaluationCache,
            refreshStrategy: config.refreshStrategy,
        }, config.experimentationOptions);
    }
    async initialize(context) {
        this.status = server_sdk_1.ProviderStatus.NOT_READY;
        try {
            await this.client.initialize();
            await this.client.eval(context || {});
            this.status = server_sdk_1.ProviderStatus.READY;
            this.events.emit(server_sdk_1.ProviderEvents.Ready, { message: 'Provider ready' });
        }
        catch (error) {
            this.status = server_sdk_1.ProviderStatus.ERROR;
            const message = error instanceof Error ? error.message : 'Initialization failed';
            this.events.emit(server_sdk_1.ProviderEvents.Error, { message, errorCode: server_sdk_1.ErrorCode.PROVIDER_NOT_READY });
            throw error;
        }
    }
    async onClose() {
        this.status = server_sdk_1.ProviderStatus.NOT_READY;
        this.processedContextCache = new WeakMap();
        await this.client.close();
    }
    async evaluateFlag(flagKey, defaultValue, context, type) {
        let processedContext = this.processedContextCache.get(context);
        if (!processedContext) {
            processedContext = this.filterContext(context);
            this.processedContextCache.set(context, processedContext);
        }
        const config = await this.client.eval(processedContext);
        const value = (0, utils_1.getNestedValue)(config, flagKey);
        const converter = TYPE_CONVERTERS[type];
        return converter(value, defaultValue);
    }
    filterContext(context) {
        const filtered = {};
        for (const [key, value] of Object.entries(context)) {
            if (key.startsWith('__') || key === 'targetingKey' || key === 'timestamp') {
                continue;
            }
            // Only include simple, serializable types
            if (typeof value === 'string' || typeof value === 'number' || typeof value === 'boolean') {
                filtered[key] = value;
            }
            else if (typeof value === 'object' && value !== null && !Array.isArray(value)) {
                try {
                    const serialized = JSON.stringify(value);
                    if (serialized.length < 1000 && Object.keys(value).length < 10) {
                        filtered[key] = value;
                    }
                }
                catch {
                    // Skip non-serializable objects
                }
            }
        }
        return filtered;
    }
    createResolver(type) {
        return async (flagKey, defaultValue, context) => {
            if (this.status !== server_sdk_1.ProviderStatus.READY && this.status !== server_sdk_1.ProviderStatus.STALE) {
                return {
                    value: defaultValue,
                    reason: 'ERROR',
                    errorCode: this.status === server_sdk_1.ProviderStatus.FATAL ? server_sdk_1.ErrorCode.PROVIDER_FATAL : server_sdk_1.ErrorCode.PROVIDER_NOT_READY,
                    errorMessage: `Provider status: ${this.status}`
                };
            }
            try {
                const value = await this.evaluateFlag(flagKey, defaultValue, context, type);
                return {
                    value,
                    reason: this.status === server_sdk_1.ProviderStatus.STALE ? 'STALE' : 'TARGETING_MATCH',
                };
            }
            catch (error) {
                return {
                    value: defaultValue,
                    reason: 'ERROR',
                    errorCode: server_sdk_1.ErrorCode.GENERAL,
                    errorMessage: error instanceof Error ? error.message : 'Evaluation failed'
                };
            }
        };
    }
    async resolveBooleanEvaluation(flagKey, defaultValue, context) {
        return this.createResolver('boolean')(flagKey, defaultValue, context);
    }
    async resolveStringEvaluation(flagKey, defaultValue, context) {
        return this.createResolver('string')(flagKey, defaultValue, context);
    }
    async resolveNumberEvaluation(flagKey, defaultValue, context) {
        return this.createResolver('number')(flagKey, defaultValue, context);
    }
    async resolveObjectEvaluation(flagKey, defaultValue, context, logger) {
        return this.createResolver('object')(flagKey, defaultValue, context);
    }
    async resolveAllConfigDetails(defaultValue, context) {
        if (this.status !== server_sdk_1.ProviderStatus.READY && this.status !== server_sdk_1.ProviderStatus.STALE) {
            return defaultValue;
        }
        try {
            const processedContext = this.processedContextCache.get(context) || this.filterContext(context);
            if (!this.processedContextCache.has(context)) {
                this.processedContextCache.set(context, processedContext);
            }
            const targetingKey = context.targetingKey;
            return await this.client.getAllConfigValue(defaultValue, processedContext, targetingKey);
        }
        catch (error) {
            console.error('Error resolving all config details:', error);
            return defaultValue;
        }
    }
    getStatus() { return this.status; }
    getConfigurationClient() { return this.client; }
}
exports.SuperpositionProvider = SuperpositionProvider;
