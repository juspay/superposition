import { SuperpositionOptions, ConfigOptions, ConfigData } from './types';
import { NativeResolver } from 'superposition-bindings';
import { ExperimentationOptions } from './types';
export declare class ConfigurationClient {
    private config;
    private resolver;
    private options;
    private currentConfigData;
    private experimentationClient?;
    private experimentationOptions?;
    private defaults;
    private smithyClient;
    constructor(config: SuperpositionOptions, resolver: NativeResolver, options?: ConfigOptions, experimentationOptions?: ExperimentationOptions);
    initialize(): Promise<void>;
    eval(queryData: Record<string, any>, filterPrefixes?: string[], targetingKey?: string): Promise<any>;
    eval<T>(queryData: Record<string, any>, filterPrefixes?: string[], targetingKey?: string): Promise<T>;
    setDefault(defaults: ConfigData): void;
    private fetchConfigData;
    getAllConfigValue(defaultValue: Record<string, any>, context: Record<string, any>, targetingKey?: string): Promise<Record<string, any>>;
    private getApplicableVariants;
    close(): Promise<void>;
}
