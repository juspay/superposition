import { SuperpositionOptions, ConfigOptions, ExperimentationOptions } from './types';
import { NativeResolver } from 'superposition-bindings';
import { CacConfig } from './configuration-options';
import { ExperimentationConfig, Experiment, ExperimentGroup } from './experimentation-options';

export class SuperpositionClient {
    private cacConfig: CacConfig;
    private experimentationConfig?: ExperimentationConfig;

    constructor(
        config: SuperpositionOptions,
        resolver: NativeResolver,
        options: ConfigOptions = {},
        experimentationOptions?: ExperimentationOptions
    ) {
        this.cacConfig = new CacConfig(config, resolver, options, experimentationOptions);
        
        if (experimentationOptions) {
            this.experimentationConfig = new ExperimentationConfig(config, experimentationOptions);
        }
    }

    async initialize(): Promise<void> {
        await this.cacConfig.initialize();
        if (this.experimentationConfig) {
            await this.experimentationConfig.initialize();
        }
    }

    async eval(queryData: Record<string, any>, filterPrefixes?: string[], targetingKey?: string): Promise<any>;
    async eval<T>(queryData: Record<string, any>, filterPrefixes?: string[], targetingKey?: string): Promise<T>;
    async eval(queryData: Record<string, any>, filterPrefixes?: string[], targetingKey?: string): Promise<any> {
        return this.cacConfig.eval(queryData, filterPrefixes, targetingKey);
    }

    async getAllConfigValue(
        defaultValue: Record<string, any>,
        context: Record<string, any>,
        targetingKey?: string
    ): Promise<Record<string, any>> {
        return this.cacConfig.getAllConfigValue(defaultValue, context, targetingKey);
    }

    setDefault(defaults: any): void {
        this.cacConfig.setDefault(defaults);
    }

    // Expose getApplicableVariants on the provider layer
    async getApplicableVariants(
        queryData: Record<string, any>,
        identifier: string,
        filterPrefixes?: string[]
    ): Promise<string[]> {
        if (!this.experimentationConfig) {
            return [];
        }
        
        const experiments = await this.experimentationConfig.getExperiments();
        return this.cacConfig.getApplicableVariants(experiments, queryData, identifier, filterPrefixes);
    }

    // Expose experiment groups functionality
    async getExperimentGroups(): Promise<ExperimentGroup[]> {
        if (!this.experimentationConfig) {
            return [];
        }
        return this.experimentationConfig.getExperimentGroups();
    }

    async getExperiments(): Promise<Experiment[]> {
        if (!this.experimentationConfig) {
            return [];
        }
        return this.experimentationConfig.getExperiments();
    }

    async close(): Promise<void> {
        await this.cacConfig.close();
        if (this.experimentationConfig) {
            await this.experimentationConfig.close();
        }
    }
}