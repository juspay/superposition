export * from './types';

// Export main classes
export { ConfigurationClient } from './configuration-client';
export { ExperimentationClient } from './experiment-client';
export { NativeResolver } from './native-resolver';
export {
    SuperpositionProvider,
    SuperpositionProviderOptions
} from './superposition-provider';


export const ENV = {
    baseUrl: "http://127.0.0.1:8080",
    org_id: "localorg",
    workspace_id: "test",
    token: "12345678",
};