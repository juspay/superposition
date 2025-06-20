// src/demo-app/enhanced-server.ts
import express from 'express';
import cors from 'cors';
import path from 'path';
import { Server as SocketIOServer } from 'socket.io';
import { createServer } from 'http';
import { OpenFeature, Provider, ResolutionDetails, EvaluationContext, ProviderStatus, JsonValue, ProviderEvents } from '@openfeature/server-sdk';

// Import your existing SuperpositionProvider with new Builder pattern - Fixed path
import { SuperpositionProvider, SuperpositionProviderBuilder } from '../src/resolver-client/superposition-provider';

// Enhanced LaunchDarkly Mock Provider with cache simulation
class LaunchDarklyProvider implements Provider {
    readonly metadata = { name: 'LaunchDarkly', slug: 'launchdarkly' };
    status = ProviderStatus.READY;

    // Simulated cache
    private cache = new Map<string, { value: any, timestamp: number, hits: number }>();
    private cacheStats = { hits: 0, misses: 0, size: 0 };

    private flags: Record<string, any> = {
        'key2': 'launchdarkly-value',
        'feature.enabled': true,
        'api.timeout': 8000,
        'ui.theme': 'dark',
        'experiment.checkout': 'variant-b'
    };

    async resolveBooleanEvaluation(flagKey: string, defaultValue: boolean, context: EvaluationContext): Promise<ResolutionDetails<boolean>> {
        const cached = this.getFromCache(flagKey, context);
        if (cached) {
            return { value: cached, reason: 'CACHED', variant: 'cached' };
        }

        const flagValue = this.flags[flagKey];
        const value = this.getUserVariant(flagKey, context, flagValue, defaultValue);
        this.setCache(flagKey, context, value);

        return {
            value: typeof value === 'boolean' ? value : defaultValue,
            reason: 'TARGETING_MATCH',
            variant: this.getVariant(flagKey, context)
        };
    }

    async resolveStringEvaluation(flagKey: string, defaultValue: string, context: EvaluationContext): Promise<ResolutionDetails<string>> {
        const cached = this.getFromCache(flagKey, context);
        if (cached) {
            return { value: cached, reason: 'CACHED', variant: 'cached' };
        }

        const flagValue = this.flags[flagKey];
        const value = this.getUserVariant(flagKey, context, flagValue, defaultValue);
        this.setCache(flagKey, context, value);

        return {
            value: typeof value === 'string' ? value : defaultValue,
            reason: 'TARGETING_MATCH',
            variant: this.getVariant(flagKey, context)
        };
    }

    async resolveNumberEvaluation(flagKey: string, defaultValue: number, context: EvaluationContext): Promise<ResolutionDetails<number>> {
        const cached = this.getFromCache(flagKey, context);
        if (cached) {
            return { value: cached, reason: 'CACHED', variant: 'cached' };
        }

        const flagValue = this.flags[flagKey];
        const value = this.getUserVariant(flagKey, context, flagValue, defaultValue);
        this.setCache(flagKey, context, value);

        return {
            value: typeof value === 'number' ? value : defaultValue,
            reason: 'TARGETING_MATCH',
            variant: this.getVariant(flagKey, context)
        };
    }

    async resolveObjectEvaluation<T extends JsonValue>(flagKey: string, defaultValue: T, context: EvaluationContext): Promise<ResolutionDetails<T>> {
        const cached = this.getFromCache(flagKey, context);
        if (cached) {
            return { value: cached, reason: 'CACHED', variant: 'cached' };
        }

        const flagValue = this.flags[flagKey];
        const value = this.getUserVariant(flagKey, context, flagValue, defaultValue);
        this.setCache(flagKey, context, value);

        return {
            value: (typeof value === 'object' && value !== null) ? value as T : defaultValue,
            reason: 'TARGETING_MATCH',
            variant: this.getVariant(flagKey, context)
        };
    }

    private getUserVariant(flagKey: string, context: EvaluationContext, flagValue: any, defaultValue: any): any {
        const userId = context.userId || 'anonymous';
        const hash = this.hash(userId + flagKey);

        if (flagKey === 'feature.enabled') {
            return hash % 2 === 0; // 50/50 split
        }

        return flagValue !== undefined ? flagValue : defaultValue;
    }

    private getVariant(flagKey: string, context: EvaluationContext): string {
        const userId = context.userId || 'anonymous';
        const hash = this.hash(userId + flagKey);
        return hash % 2 === 0 ? 'control' : 'treatment';
    }

    private hash(str: string): number {
        let hash = 0;
        for (let i = 0; i < str.length; i++) {
            const char = str.charCodeAt(i);
            hash = ((hash << 5) - hash) + char;
            hash = hash & hash;
        }
        return Math.abs(hash);
    }

    private getCacheKey(flagKey: string, context: EvaluationContext): string {
        return `${flagKey}:${JSON.stringify(context)}`;
    }

    private getFromCache(flagKey: string, context: EvaluationContext): any | null {
        const key = this.getCacheKey(flagKey, context);
        const cached = this.cache.get(key);

        if (cached && Date.now() - cached.timestamp < 30000) { // 30s TTL
            cached.hits++;
            this.cacheStats.hits++;
            return cached.value;
        }

        if (cached) {
            this.cache.delete(key);
        }

        this.cacheStats.misses++;
        return null;
    }

    private setCache(flagKey: string, context: EvaluationContext, value: any): void {
        const key = this.getCacheKey(flagKey, context);
        this.cache.set(key, { value, timestamp: Date.now(), hits: 0 });
        this.cacheStats.size = this.cache.size;
    }

    getCacheStats() {
        return {
            ...this.cacheStats,
            size: this.cache.size,
            entries: Array.from(this.cache.entries()).map(([key, data]) => ({
                key,
                value: data.value,
                age: Date.now() - data.timestamp,
                hits: data.hits
            }))
        };
    }

    clearCache() {
        this.cache.clear();
        this.cacheStats = { hits: 0, misses: 0, size: 0 };
    }
}

// Enhanced Flagsmith Mock Provider
class FlagsmithProvider implements Provider {
    readonly metadata = { name: 'Flagsmith', slug: 'flagsmith' };
    status = ProviderStatus.READY;

    private cache = new Map<string, { value: any, timestamp: number, hits: number }>();
    private cacheStats = { hits: 0, misses: 0, size: 0 };

    private flags: Record<string, any> = {
        'key2': 'flagsmith-value',
        'feature.enabled': false,
        'api.timeout': 5000,
        'ui.theme': 'light',
        'experiment.checkout': 'variant-a'
    };

    async resolveBooleanEvaluation(flagKey: string, defaultValue: boolean, context: EvaluationContext): Promise<ResolutionDetails<boolean>> {
        const cached = this.getFromCache(flagKey, context);
        if (cached) {
            return { value: cached, reason: 'CACHED', variant: 'cached' };
        }

        const flagValue = this.flags[flagKey];
        const value = this.getSegmentValue(flagKey, context, flagValue, defaultValue);
        this.setCache(flagKey, context, value);

        return {
            value: typeof value === 'boolean' ? value : defaultValue,
            reason: 'TARGETING_MATCH',
            variant: this.getSegment(context)
        };
    }

    async resolveStringEvaluation(flagKey: string, defaultValue: string, context: EvaluationContext): Promise<ResolutionDetails<string>> {
        const cached = this.getFromCache(flagKey, context);
        if (cached) {
            return { value: cached, reason: 'CACHED', variant: 'cached' };
        }

        const flagValue = this.flags[flagKey];
        const value = this.getSegmentValue(flagKey, context, flagValue, defaultValue);
        this.setCache(flagKey, context, value);

        return {
            value: typeof value === 'string' ? value : defaultValue,
            reason: 'TARGETING_MATCH',
            variant: this.getSegment(context)
        };
    }

    async resolveNumberEvaluation(flagKey: string, defaultValue: number, context: EvaluationContext): Promise<ResolutionDetails<number>> {
        const cached = this.getFromCache(flagKey, context);
        if (cached) {
            return { value: cached, reason: 'CACHED', variant: 'cached' };
        }

        const flagValue = this.flags[flagKey];
        const value = this.getSegmentValue(flagKey, context, flagValue, defaultValue);
        this.setCache(flagKey, context, value);

        return {
            value: typeof value === 'number' ? value : defaultValue,
            reason: 'TARGETING_MATCH',
            variant: this.getSegment(context)
        };
    }

    async resolveObjectEvaluation<T extends JsonValue>(flagKey: string, defaultValue: T, context: EvaluationContext): Promise<ResolutionDetails<T>> {
        const cached = this.getFromCache(flagKey, context);
        if (cached) {
            return { value: cached, reason: 'CACHED', variant: 'cached' };
        }

        const flagValue = this.flags[flagKey];
        const value = this.getSegmentValue(flagKey, context, flagValue, defaultValue);
        this.setCache(flagKey, context, value);

        return {
            value: (typeof value === 'object' && value !== null) ? value as T : defaultValue,
            reason: 'TARGETING_MATCH',
            variant: this.getSegment(context)
        };
    }

    private getSegmentValue(flagKey: string, context: EvaluationContext, flagValue: any, defaultValue: any): any {
        // Segment based on OS
        if (context.os === 'ios' && flagKey === 'feature.enabled') {
            return true; // iOS users get enabled features
        }

        return flagValue !== undefined ? flagValue : defaultValue;
    }

    private getSegment(context: EvaluationContext): string {
        if (context.os === 'ios') return 'ios-segment';
        if (context.os === 'android') return 'android-segment';
        return 'web-segment';
    }

    private getCacheKey(flagKey: string, context: EvaluationContext): string {
        return `${flagKey}:${JSON.stringify(context)}`;
    }

    private getFromCache(flagKey: string, context: EvaluationContext): any | null {
        const key = this.getCacheKey(flagKey, context);
        const cached = this.cache.get(key);

        if (cached && Date.now() - cached.timestamp < 60000) { // 60s TTL
            cached.hits++;
            this.cacheStats.hits++;
            return cached.value;
        }

        if (cached) {
            this.cache.delete(key);
        }

        this.cacheStats.misses++;
        return null;
    }

    private setCache(flagKey: string, context: EvaluationContext, value: any): void {
        const key = this.getCacheKey(flagKey, context);
        this.cache.set(key, { value, timestamp: Date.now(), hits: 0 });
        this.cacheStats.size = this.cache.size;
    }

    getCacheStats() {
        return {
            ...this.cacheStats,
            size: this.cache.size,
            entries: Array.from(this.cache.entries()).map(([key, data]) => ({
                key,
                value: data.value,
                age: Date.now() - data.timestamp,
                hits: data.hits
            }))
        };
    }

    clearCache() {
        this.cache.clear();
        this.cacheStats = { hits: 0, misses: 0, size: 0 };
    }
}

// Enhanced Demo Server
class EnhancedDemoServer {
    private app = express();
    private server = createServer(this.app);
    private io = new SocketIOServer(this.server, {
        cors: { origin: "*", methods: ["GET", "POST"] }
    });

    private providers: Record<string, Provider & { getCacheStats?: () => any, clearCache?: () => void }> = {};
    private currentProvider = 'superposition';
    private eventHistory: Array<{ timestamp: number, provider: string, event: string, data: any }> = [];

    constructor() {
        this.setupProviders();
        this.setupExpress();
        this.setupWebSocket();
    }

    private async setupProviders() {
        // Superposition Provider using new Builder pattern with enhanced configuration
        this.providers.superposition = SuperpositionProviderBuilder.builder()
            .endpoint('http://localhost:8080')
            .workspaceId('dev')
            .orgId('localorg')
            .token('12345678')
            .fallbackConfig({
                'key2': 'value2',
                'feature.enabled': true,
                'api.timeout': 5000,
                'ui.theme': 'light',
                'experiment.checkout': 'control'
            })
            .refreshStrategy({
                polling: {
                    enabled: true,
                    interval: 30000,
                    timeout: 5000,
                    onError: 'stale'
                }
            })
            .cache({ ttl: 60000, size: 1000 })
            .evaluationCache({ ttl: 300000, size: 1000 })
            .experimentationOptions({
                enabled: true,
                filterPrefixes: ['exp_', 'test_'],
                refreshStrategy: {
                    polling: {
                        enabled: true,
                        interval: 30000,
                        timeout: 5000
                    }
                }
            })
            .build();

        // Enhanced mock providers
        this.providers.launchdarkly = new LaunchDarklyProvider();
        this.providers.flagsmith = new FlagsmithProvider();

        // Set up event listeners for all providers
        this.setupProviderEvents();

        // Initialize Superposition
        try {
            const superProvider = this.providers.superposition as SuperpositionProvider;
            if (superProvider && typeof superProvider.initialize === 'function') {
                await superProvider.initialize();
                console.log('✅ Superposition initialized successfully');
            }
        } catch (error) {
            console.warn('Superposition initialization failed:', error.message);
            this.logEvent('superposition', 'initialization_failed', { error: error.message });
        }

        // Set initial provider
        OpenFeature.setProvider(this.providers[this.currentProvider]);
    }

    private setupProviderEvents() {
        Object.entries(this.providers).forEach(([name, provider]) => {
            if (provider.events) {
                // Listen to all OpenFeature provider events
                provider.events.addHandler(ProviderEvents.Ready, (eventDetails) => {
                    this.logEvent(name, 'ready', eventDetails);
                });

                provider.events.addHandler(ProviderEvents.Error, (eventDetails) => {
                    this.logEvent(name, 'error', eventDetails);
                });

                provider.events.addHandler(ProviderEvents.Stale, (eventDetails) => {
                    this.logEvent(name, 'stale', eventDetails);
                });

                provider.events.addHandler(ProviderEvents.ConfigurationChanged, (eventDetails) => {
                    this.logEvent(name, 'config_changed', eventDetails);
                });
            }
        });
    }

    private logEvent(provider: string, event: string, data: any) {
        const eventData = {
            timestamp: Date.now(),
            provider,
            event,
            data
        };

        this.eventHistory.unshift(eventData);

        // Keep only last 50 events
        if (this.eventHistory.length > 50) {
            this.eventHistory = this.eventHistory.slice(0, 50);
        }

        // Emit to all connected clients
        this.io.emit('provider-event', eventData);

        console.log(`🎯 Provider Event [${provider}]: ${event}`, data);
    }

    private setupWebSocket() {
        this.io.on('connection', (socket) => {
            console.log('Client connected to WebSocket');

            // Send current event history to new client
            socket.emit('event-history', this.eventHistory);

            socket.on('disconnect', () => {
                console.log('Client disconnected from WebSocket');
            });
        });
    }

    private setupExpress() {
        this.app.use(cors());
        this.app.use(express.json());
        this.app.use(express.static(path.join(__dirname, 'public')));

        // Existing API Routes
        this.app.get('/api/providers', (req, res) => {
            res.json({
                current: this.currentProvider,
                available: Object.keys(this.providers),
                status: this.providers[this.currentProvider].status
            });
        });

        this.app.post('/api/switch-provider', async (req, res) => {
            const { provider } = req.body;

            if (!this.providers[provider]) {
                return res.status(400).json({ error: 'Invalid provider' });
            }

            const oldProvider = this.providers[this.currentProvider];
            if (oldProvider && typeof oldProvider.onClose === 'function') {
                await oldProvider.onClose();
            }

            this.currentProvider = provider;
            OpenFeature.setProvider(this.providers[provider]);

            this.logEvent(provider, 'provider_switched', { from: this.currentProvider, to: provider });

            res.json({
                success: true,
                currentProvider: provider,
                status: this.providers[provider].status
            });
        });

        this.app.post('/api/evaluate-flags', async (req, res) => {
            const { context, evaluationOptions } = req.body;
            const client = OpenFeature.getClient('demo-client');

            try {
                let evaluationContext: EvaluationContext = context as EvaluationContext; // Explicitly type for clarity and safety
                let experimentationContext: EvaluationContext | null = null;

                // Enhanced experimentation context handling for Superposition
                if (this.currentProvider === 'superposition') {
                    const superProvider = this.providers.superposition as SuperpositionProvider;
                    if (superProvider.isExperimentationEnabled()) {
                        const expResult = await superProvider.getExperimentationContext({
                            ...context,
                            toss: evaluationOptions?.toss
                        });
                        experimentationContext = expResult; // Store for reporting, can be null
                        if (expResult) { // Only update evaluationContext if a valid context is returned
                            evaluationContext = expResult;
                        }
                    }
                }

                const flags = {
                    stringFlags: {
                        'key2': await client.getStringDetails('key2', 'default', evaluationContext),
                        'ui.theme': await client.getStringDetails('ui.theme', 'default', evaluationContext),
                        'experiment.checkout': await client.getStringDetails('experiment.checkout', 'control', evaluationContext)
                    },
                    booleanFlags: {
                        'feature.enabled': await client.getBooleanDetails('feature.enabled', false, evaluationContext)
                    },
                    numberFlags: {
                        'api.timeout': await client.getNumberDetails('api.timeout', 5000, evaluationContext)
                    }
                };

                res.json({
                    provider: this.currentProvider,
                    providerStatus: this.providers[this.currentProvider].status,
                    context: evaluationContext,
                    originalContext: context,
                    experimentationContext,
                    flags,
                    experimentationEnabled: this.currentProvider === 'superposition' ?
                        (this.providers.superposition as SuperpositionProvider).isExperimentationEnabled() : false,
                    evaluationOptions
                });
            } catch (error) {
                res.status(500).json({ error: 'Flag evaluation failed', details: error.message });
            }
        });

        // Enhanced Cache Management Routes
        this.app.get('/api/cache-stats', (req, res) => {
            const provider = this.providers[this.currentProvider];

            if (provider.getCacheStats) {
                res.json({
                    provider: this.currentProvider,
                    stats: provider.getCacheStats()
                });
            } else if (this.currentProvider === 'superposition') {
                // For Superposition, we need to access internal client stats
                const superProvider = provider as SuperpositionProvider;
                const configClient = superProvider.getConfigurationClient();
                const expClient = superProvider.getExperimentationClient();

                res.json({
                    provider: this.currentProvider,
                    stats: {
                        status: superProvider.getStatus(),
                        configurationClient: 'Available (internal caching)',
                        experimentationClient: expClient ? {
                            enabled: true,
                            stats: expClient.getCacheStats ? expClient.getCacheStats() : 'Available (internal caching)'
                        } : 'Disabled',
                        experimentationEnabled: superProvider.isExperimentationEnabled(),
                        evaluationCache: 'Available (internal)',
                        note: 'Superposition uses internal caching with enhanced strategies'
                    }
                });
            } else {
                res.json({
                    provider: this.currentProvider,
                    stats: { message: 'Cache stats not available for this provider' }
                });
            }
        });

        this.app.post('/api/clear-cache', async (req, res) => {
            const provider = this.providers[this.currentProvider];

            if (provider.clearCache) {
                provider.clearCache();
                this.logEvent(this.currentProvider, 'cache_cleared', {});
                res.json({ success: true, message: 'Cache cleared' });
            } else if (this.currentProvider === 'superposition') {
                const superProvider = provider as SuperpositionProvider;
                try {
                    await superProvider.refreshConfiguration();
                    // Also refresh experimentation cache if available
                    const expClient = superProvider.getExperimentationClient();
                    if (expClient && expClient.refreshCache) {
                        await expClient.refreshCache();
                    }
                    this.logEvent(this.currentProvider, 'cache_refreshed', {});
                    res.json({ success: true, message: 'Cache refresh completed' });
                } catch (error) {
                    res.status(500).json({ error: 'Cache refresh failed', details: error.message });
                }
            } else {
                res.status(400).json({ error: 'Cache clearing not supported for this provider' });
            }
        });

        // Enhanced Experimentation Routes
        this.app.get('/api/experimentation/status', (req, res) => {
            if (this.currentProvider === 'superposition') {
                const superProvider = this.providers.superposition as SuperpositionProvider;
                const expClient = superProvider.getExperimentationClient();

                res.json({
                    enabled: superProvider.isExperimentationEnabled(),
                    status: superProvider.getStatus(),
                    experimentationClient: expClient ? {
                        available: true,
                        stats: expClient.getCacheStats ? expClient.getCacheStats() : {}
                    } : null
                });
            } else {
                res.json({
                    enabled: false,
                    message: 'Experimentation only available for Superposition provider'
                });
            }
        });

        this.app.post('/api/experimentation/evaluate', async (req, res) => {
            const { context, toss, options } = req.body;

            if (this.currentProvider !== 'superposition') {
                return res.status(400).json({ error: 'Experimentation only available for Superposition provider' });
            }

            try {
                const superProvider = this.providers.superposition as SuperpositionProvider;
                const expClient = superProvider.getExperimentationClient();

                if (!expClient) {
                    return res.status(400).json({ error: 'Experimentation not enabled' });
                }

                const result = expClient.evaluateExperiments(context, toss, options);
                res.json({
                    success: true,
                    result,
                    context,
                    evaluationOptions: { toss, ...options }
                });
            } catch (error) {
                res.status(500).json({ error: 'Experimentation evaluation failed', details: error.message });
            }
        });

        // Event History Route
        this.app.get('/api/events', (req, res) => {
            res.json({
                events: this.eventHistory,
                total: this.eventHistory.length
            });
        });

        this.app.delete('/api/events', (req, res) => {
            this.eventHistory = [];
            res.json({ success: true, message: 'Event history cleared' });
        });

        // Provider Control Routes
        this.app.post('/api/provider/:provider/refresh', async (req, res) => {
            const { provider: providerName } = req.params;
            const provider = this.providers[providerName];

            if (!provider) {
                return res.status(404).json({ error: 'Provider not found' });
            }

            try {
                if (providerName === 'superposition') {
                    const superProvider = provider as SuperpositionProvider;
                    if (superProvider.refreshConfiguration) {
                        await superProvider.refreshConfiguration();
                        this.logEvent(providerName, 'manual_refresh', {});
                        res.json({
                            success: true,
                            message: 'Provider refreshed',
                            status: superProvider.getStatus(),
                            experimentationEnabled: superProvider.isExperimentationEnabled()
                        });
                    } else {
                        res.status(400).json({ error: 'Refresh not supported' });
                    }
                } else {
                    res.json({ success: true, message: 'Mock provider - no refresh needed' });
                }
            } catch (error) {
                res.status(500).json({ error: 'Refresh failed', details: error.message });
            }
        });

        // Serve UI
        this.app.get('/', (req, res) => {
            res.sendFile(path.join(__dirname, 'public/index.html'));
        });
    }

    start(port = 3000) {
        this.server.listen(port, () => {
            console.log(`🚀 Enhanced OpenFeature Demo running at http://localhost:${port}`);
            console.log(`📊 Current provider: ${this.currentProvider}`);
            console.log(`🔄 Available providers: ${Object.keys(this.providers).join(', ')}`);
            console.log(`🎯 WebSocket enabled for real-time events`);
            console.log(`🧪 Enhanced experimentation support with new evaluation options`);
        });
    }
}

// Start the enhanced server
const server = new EnhancedDemoServer();
server.start();