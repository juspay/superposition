// --- Legacy provider (kept for backward compatibility) ---
export { SuperpositionProvider, SuperpositionProviderOptions } from './superposition-provider';
export { ConfigurationClient } from './configuration-client';
export { ExperimentationClient, Experiment, Variant } from './experimentation-client';

// --- New architecture (parity with the Rust/Python/Java providers) ---
// Note: the canonical types below (SuperpositionOptions, RefreshStrategy, ...) intentionally supersede
// the legacy same-named aliases in ./types, which the legacy provider imports directly rather than
// via this barrel.
export { SuperpositionError, ErrorCode } from './errors';
export {
    AuthMethod,
    TokenAuth,
    BasicAuth,
    SuperpositionOptions,
    validateOptions,
    sdkAuthConfig,
    RefreshStrategy,
    PollingStrategy,
    OnDemandStrategy,
    WatchStrategy,
    ManualStrategy,
    defaultPollingStrategy,
    defaultOnDemandStrategy,
    defaultWatchStrategy,
} from './options';
export {
    FetchResponse,
    BaseDataSource,
    Config,
    ConfigData,
    ExperimentConfig,
    ExperimentData,
    SuperpositionDataSource,
} from './data-source';
export { HttpDataSource } from './http-data-source';
export { FileDataSource } from './file-data-source';
export { AllFeatureProvider, FeatureExperimentMeta } from './interfaces';
export { LocalResolutionProvider } from './local-provider';
export { SuperpositionAPIProvider } from './remote-provider';
