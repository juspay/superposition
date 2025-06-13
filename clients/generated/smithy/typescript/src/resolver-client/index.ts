import { ConfigurationClient } from './configuration-client';
import { NativeResolver } from './native-resolver';
import { SuperpositionConfig, CACClientOptions } from './types';

export class SuperpositionClient {
    private config: SuperpositionConfig;
    private resolver: NativeResolver;

    constructor(config: SuperpositionConfig) {
        this.config = config;
        this.resolver = new NativeResolver();
    }

    getConfigurationClient(options: CACClientOptions = {}): ConfigurationClient {
        return new ConfigurationClient(this.config, this.resolver, options);
    }

}

// Export types for external use
export * from './types';
export { ConfigurationClient } from './configuration-client';

// Environment setup
export const ENV = {
    baseUrl: "http://127.0.0.1:8080",
    org_id: "localorg",
    workspace_id: "test",
};

// Default client instance
const config: SuperpositionConfig = {
    endpoint: ENV.baseUrl,
    token: {
        token: "12345678",
    },
    org_id: ENV.org_id,
    workspace_id: ENV.workspace_id,
};

export const superpositionClient = new SuperpositionClient(config);