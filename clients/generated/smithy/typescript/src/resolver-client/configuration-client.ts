import { SuperpositionConfig, CACClientOptions, ConfigData, EvalOptions } from './types';
import { NativeResolver } from './native-resolver';

export class ConfigurationClient {
    private config: SuperpositionConfig;
    private resolver: NativeResolver;
    private options: CACClientOptions;
    private cachedConfig: ConfigData | null = null;
    private lastFetchTime: number = 0;
    private evaluationCache = new Map<string, { value: any, timestamp: number }>();
    private defaults: ConfigData | null = null;

    constructor(
        config: SuperpositionConfig,
        resolver: NativeResolver,
        options: CACClientOptions = {}
    ) {
        this.config = config;
        this.resolver = resolver;
        this.options = options;
        this.defaults = options.defaults || null;
    }

    // Core eval method with multiple overloads
    async eval(context: Record<string, any>): Promise<any>;
    async eval(context: Record<string, any>, refresh: boolean): Promise<any>;
    async eval(context: Record<string, any>, filterKeys: string[]): Promise<any>;
    async eval<T>(context: Record<string, any>): Promise<T>;
    async eval<T>(context: Record<string, any>, refresh: boolean): Promise<T>;
    async eval<T>(context: Record<string, any>, filterKeys: string[]): Promise<T>;

    async eval(
        context: Record<string, any>,
        optionsOrRefreshOrFilterKeys?: boolean | string[] | EvalOptions
    ): Promise<any> {
        let options: EvalOptions = {};

        // Handle different parameter types
        if (typeof optionsOrRefreshOrFilterKeys === 'boolean') {
            options.refresh = optionsOrRefreshOrFilterKeys;
        } else if (Array.isArray(optionsOrRefreshOrFilterKeys)) {
            options.filterKeys = optionsOrRefreshOrFilterKeys;
        } else if (optionsOrRefreshOrFilterKeys) {
            options = optionsOrRefreshOrFilterKeys;
        }

        try {
            // Check evaluation cache first
            const cacheKey = this.generateEvaluationCacheKey(context, options.filterKeys);
            const cached = this.getFromEvaluationCache(cacheKey);

            if (cached && !options.refresh) {
                return cached;
            }

            // Get configuration data
            const configData = await this.getConfigData(options.refresh);

            // Apply filter keys if provided
            const filteredConfig = this.applyFilterKeys(configData, options.filterKeys);

            // Resolve configuration using native resolver
            const result = this.resolver.resolveConfig(
                filteredConfig.defaultConfigs,
                filteredConfig.contexts,
                filteredConfig.overrides,
                context,
                options.mergeStrategy || 'merge'
            );

            // Cache the evaluation result
            this.setEvaluationCache(cacheKey, result);

            return result;
        } catch (error) {
            // Use defaults if available
            if (this.defaults) {
                const filteredDefaults = this.applyFilterKeys(this.defaults, options.filterKeys);
                return this.resolver.resolveConfig(
                    filteredDefaults.defaultConfigs,
                    filteredDefaults.contexts,
                    filteredDefaults.overrides,
                    context,
                    options.mergeStrategy || 'merge'
                );
            }
            throw error;
        }
    }

    // OpenFeature-compatible methods
    async getBooleanValue(
        key: string,
        defaultValue: boolean,
        context: Record<string, any>,
        options: { refresh?: boolean } = {}
    ): Promise<boolean> {
        const config = await this.eval(context, options.refresh || false);
        const value = this.getNestedValue(config, key);
        return this.convertToBoolean(value, defaultValue);
    }

    async getStringValue(
        key: string,
        defaultValue: string,
        context: Record<string, any>,
        options: { refresh?: boolean } = {}
    ): Promise<string> {
        const config = await this.eval(context, options.refresh || false);
        const value = this.getNestedValue(config, key);
        return this.convertToString(value, defaultValue);
    }

    async getIntegerValue(
        key: string,
        defaultValue: number,
        context: Record<string, any>,
        options: { refresh?: boolean } = {}
    ): Promise<number> {
        const config = await this.eval(context, options.refresh || false);
        const value = this.getNestedValue(config, key);
        return this.convertToInteger(value, defaultValue);
    }

    async getObjectValue<T>(
        key: string,
        defaultValue: T,
        context: Record<string, any>,
        options: { refresh?: boolean } = {}
    ): Promise<T> {
        const config = await this.eval(context, options.refresh || false);
        const value = this.getNestedValue(config, key);
        return this.convertToObject(value, defaultValue);
    }

    // Additional convenience methods
    async getAll<T>(context: Record<string, any>): Promise<T> {
        return this.eval<T>(context);
    }

    async getFiltered(context: Record<string, any>, filterKeys: string[]): Promise<any> {
        return this.eval(context, filterKeys);
    }

    async refreshCache(): Promise<void> {
        await this.updateCache();
    }

    setDefault(defaults: ConfigData): void {
        this.defaults = defaults;
    }

    async updateCache(partialConfig: Record<string, any> = {}): Promise<void> {
        if (Object.keys(partialConfig).length === 0) {
            // Full refresh
            await this.fetchConfigData();
        } else {
            // Partial update - merge with existing config
            if (this.cachedConfig) {
                this.cachedConfig = { ...this.cachedConfig, ...partialConfig };
                this.clearEvaluationCache();
            }
        }
    }

    // Private helper methods
    private async getConfigData(forceRefresh = false): Promise<ConfigData> {
        const cacheTtl = this.options.cache?.ttl || 60000; // Default 1 minute
        const now = Date.now();

        // Use cached data if available and not expired (unless force refresh)
        if (!forceRefresh && this.cachedConfig && now - this.lastFetchTime < cacheTtl) {
            return this.cachedConfig;
        }

        return this.fetchConfigData();
    }

    private async fetchConfigData(): Promise<ConfigData> {
        const headers: Record<string, string> = {
            'Content-Type': 'application/json'
        };

        if (this.config.token?.token) {
            headers['Authorization'] = `Bearer ${this.config.token.token}`;
        }

        if (this.config.org_id) {
            headers['X-Organization-ID'] = this.config.org_id;
        }

        if (this.config.workspace_id) {
            headers['X-Workspace-ID'] = this.config.workspace_id;
        }

        const response = await fetch(`${this.config.endpoint}/config`, { headers });

        if (!response.ok) {
            throw new Error(`Failed to fetch configuration: ${response.status} ${response.statusText}`);
        }

        const data = await response.json() as ConfigData;
        this.cachedConfig = data as ConfigData;
        this.lastFetchTime = Date.now();
        this.clearEvaluationCache(); // Clear evaluation cache when config updates

        return data;
    }

    private applyFilterKeys(config: ConfigData, filterKeys?: string[]): ConfigData {
        if (!filterKeys || filterKeys.length === 0) {
            return config;
        }

        // Filter defaultConfigs
        const filteredDefaultConfigs: Record<string, any> = {};
        for (const [key, value] of Object.entries(config.defaultConfigs)) {
            if (filterKeys.some(filterKey => key.startsWith(filterKey))) {
                filteredDefaultConfigs[key] = value;
            }
        }

        // Filter overrides
        const filteredOverrides: Record<string, Record<string, any>> = {};
        for (const [overrideKey, overrideValue] of Object.entries(config.overrides)) {
            const filteredOverride: Record<string, any> = {};
            for (const [key, value] of Object.entries(overrideValue)) {
                if (filterKeys.some(filterKey => key.startsWith(filterKey))) {
                    filteredOverride[key] = value;
                }
            }
            if (Object.keys(filteredOverride).length > 0) {
                filteredOverrides[overrideKey] = filteredOverride;
            }
        }

        return {
            defaultConfigs: filteredDefaultConfigs,
            contexts: config.contexts, // Contexts remain the same
            overrides: filteredOverrides
        };
    }

    private getNestedValue(obj: any, key: string): any {
        const keyParts = key.split('.');
        let value = obj;

        for (const part of keyParts) {
            if (value === undefined || value === null || typeof value !== 'object') {
                return undefined;
            }
            value = value[part];
        }

        return value;
    }

    private convertToBoolean(value: any, defaultValue: boolean): boolean {
        if (typeof value === 'boolean') return value;
        if (typeof value === 'string') {
            if (value.toLowerCase() === 'true') return true;
            if (value.toLowerCase() === 'false') return false;
        }
        if (typeof value === 'number') return value !== 0;
        return defaultValue;
    }

    private convertToString(value: any, defaultValue: string): string {
        if (typeof value === 'string') return value;
        if (value !== undefined && value !== null) return String(value);
        return defaultValue;
    }

    private convertToInteger(value: any, defaultValue: number): number {
        if (typeof value === 'number' && Number.isInteger(value)) return value;
        if (typeof value === 'string') {
            const parsed = parseInt(value, 10);
            if (!isNaN(parsed)) return parsed;
        }
        return defaultValue;
    }

    private convertToObject<T>(value: any, defaultValue: T): T {
        if (value !== null && typeof value === 'object') return value as T;
        return defaultValue;
    }

    private generateEvaluationCacheKey(context: Record<string, any>, filterKeys?: string[]): string {
        return JSON.stringify({
            context,
            filterKeys: filterKeys?.sort()
        });
    }

    private getFromEvaluationCache(key: string): any | null {
        const cached = this.evaluationCache.get(key);
        if (!cached) return null;

        const ttl = this.options.evaluationCache?.ttl || 30000; // Default 30 seconds
        const now = Date.now();

        if (now - cached.timestamp > ttl) {
            this.evaluationCache.delete(key);
            return null;
        }

        return cached.value;
    }

    private setEvaluationCache(key: string, value: any): void {
        const maxSize = this.options.evaluationCache?.size || 100;

        // Remove oldest entries if cache is full
        if (this.evaluationCache.size >= maxSize) {
            const oldestKey = this.evaluationCache.keys().next().value;
            if (oldestKey !== undefined) {
                this.evaluationCache.delete(oldestKey);
            }
        }

        this.evaluationCache.set(key, {
            value,
            timestamp: Date.now()
        });
    }

    private clearEvaluationCache(): void {
        this.evaluationCache.clear();
    }
}