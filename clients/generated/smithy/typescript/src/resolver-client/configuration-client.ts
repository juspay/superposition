import { SuperpositionConfig, CACClientOptions, ConfigData, EvalOptions } from './types';
import { NativeResolver } from './native-resolver';

export class ConfigurationClient {
    private config: SuperpositionConfig;
    private resolver: NativeResolver;
    private options: CACClientOptions;
    private cachedConfigData: ConfigData | null = null;
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

    // Core eval method - simplified to match benchmark pattern
    async eval(queryData: Record<string, any>): Promise<any>;
    async eval(queryData: Record<string, any>, refresh: boolean): Promise<any>;
    async eval<T>(queryData: Record<string, any>): Promise<T>;
    async eval<T>(queryData: Record<string, any>, refresh: boolean): Promise<T>;

    async eval(
        queryData: Record<string, any>,
        refresh: boolean = false
    ): Promise<any> {
        try {
            // Check evaluation cache first
            const cacheKey = this.generateEvaluationCacheKey(queryData);
            const cached = this.getFromEvaluationCache(cacheKey);

            if (cached && !refresh) {
                console.log('Using cached evaluation result');
                return cached;
            }

            // Get configuration data from server
            const configData = await this.getConfigData(refresh);

            console.log('Config data received:', {
                hasDefaultConfigs: !!configData.default_configs && Object.keys(configData.default_configs).length > 0,
                hasContexts: !!configData.contexts && configData.contexts.length > 0,
                hasOverrides: !!configData.overrides && Object.keys(configData.overrides).length > 0
            });

            // Direct pass-through to resolver - no filtering, no modification
            // This matches your benchmark test exactly
            const result = this.resolver.resolveConfig(
                configData.default_configs || {},
                configData.contexts || [],
                configData.overrides || {},
                queryData,  // User's query data directly
                'merge'
            );

            console.log('Resolution result:', result);

            // Cache the evaluation result
            this.setEvaluationCache(cacheKey, result);

            return result;
        } catch (error) {
            console.error('Evaluation failed:', error);

            // Use defaults if available
            if (this.defaults) {
                console.log('🔄 Falling back to defaults');
                return this.resolver.resolveConfig(
                    this.defaults.default_configs || {},
                    this.defaults.contexts || [],
                    this.defaults.overrides || {},
                    queryData,
                    'merge'
                );
            }
            throw error;
        }
    }

    // OpenFeature-compatible methods
    async getBooleanValue(
        key: string,
        defaultValue: boolean,
        queryData: Record<string, any>,
        options: { refresh?: boolean } = {}
    ): Promise<boolean> {
        const config = await this.eval(queryData, options.refresh || false);
        const value = this.getNestedValue(config, key);
        return this.convertToBoolean(value, defaultValue);
    }

    async getStringValue(
        key: string,
        defaultValue: string,
        queryData: Record<string, any>,
        options: { refresh?: boolean } = {}
    ): Promise<string> {
        const config = await this.eval(queryData, options.refresh || false);
        const value = this.getNestedValue(config, key);
        return this.convertToString(value, defaultValue);
    }

    async getIntegerValue(
        key: string,
        defaultValue: number,
        queryData: Record<string, any>,
        options: { refresh?: boolean } = {}
    ): Promise<number> {
        const config = await this.eval(queryData, options.refresh || false);
        const value = this.getNestedValue(config, key);
        return this.convertToInteger(value, defaultValue);
    }

    async getObjectValue<T>(
        key: string,
        defaultValue: T,
        queryData: Record<string, any>,
        options: { refresh?: boolean } = {}
    ): Promise<T> {
        const config = await this.eval(queryData, options.refresh || false);
        const value = this.getNestedValue(config, key);
        return this.convertToObject(value, defaultValue);
    }

    async getAll<T>(queryData: Record<string, any>): Promise<T> {
        return this.eval<T>(queryData);
    }

    async refreshCache(): Promise<void> {
        await this.getConfigData(true);
    }

    setDefault(defaults: ConfigData): void {
        this.defaults = defaults;
    }

    // Simple config data management
    private async getConfigData(forceRefresh = false): Promise<ConfigData> {
        const cacheTtl = this.options.cache?.ttl || 60000; // Default 1 minute
        const now = Date.now();

        // Use cached CONFIG DATA if available and not expired
        if (!forceRefresh && this.cachedConfigData && now - this.lastFetchTime < cacheTtl) {
            console.log('Using cached config data');
            return this.cachedConfigData;
        }

        console.log('Fetching fresh config data from server');
        return this.fetchConfigData();
    }

    private async fetchConfigData(): Promise<ConfigData> {
        const headers: Record<string, string> = {
            'Content-Type': 'application/json',
            'Accept': 'application/json'
        };

        if (this.config.token?.token) {
            headers['Authorization'] = `Bearer ${this.config.token.token}`;
        }

        if (this.config.org_id) {
            headers['x-org-id'] = this.config.org_id;
        }

        if (this.config.workspace_id) {
            headers['x-tenant'] = this.config.workspace_id;
        }

        console.log('Fetching from:', `${this.config.endpoint}/config`);
        console.log('Headers:', headers);

        const response = await fetch(`${this.config.endpoint}/config`, {
            method: 'GET',
            headers
        });

        if (!response.ok) {
            const errorText = await response.text();
            console.error('Fetch failed:', response.status, errorText);
            throw new Error(`Failed to fetch configuration: ${response.status} ${response.statusText}`);
        }

        const data = await response.json() as ConfigData;
        console.log('Raw server response:', JSON.stringify(data, null, 2));

        // Validate and normalize the response
        const configData: ConfigData = {
            default_configs: data.default_configs || {},
            contexts: data.contexts || [],
            overrides: data.overrides || {}
        };

        console.log('Normalized config data:', {
            defaultConfigKeys: Object.keys(configData.default_configs),
            contextCount: configData.contexts.length,
            overrideKeys: Object.keys(configData.overrides)
        });

        // Cache the CONFIG DATA
        this.cachedConfigData = configData;
        this.lastFetchTime = Date.now();
        this.clearEvaluationCache(); // Clear evaluation cache when config updates

        return configData;
    }

    // Helper methods
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

    private generateEvaluationCacheKey(queryData: Record<string, any>): string {
        return JSON.stringify(queryData);
    }

    private getFromEvaluationCache(key: string): any | null {
        const cached = this.evaluationCache.get(key);
        if (!cached) return null;

        const ttl = this.options.evaluationCache?.ttl || 30000;
        const now = Date.now();

        if (now - cached.timestamp > ttl) {
            this.evaluationCache.delete(key);
            return null;
        }

        return cached.value;
    }

    private setEvaluationCache(key: string, value: any): void {
        const maxSize = this.options.evaluationCache?.size || 100;

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