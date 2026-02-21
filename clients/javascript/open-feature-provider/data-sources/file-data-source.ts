import { readFile } from "fs/promises";
import { existsSync } from "fs";
import path from "path";
import {
    SuperpositionDataSource,
    ConfigData,
    ExperimentData,
} from "../types";
import { NativeResolver } from "superposition-bindings";

/**
 * File-based implementation of SuperpositionDataSource
 * Reads configuration from a local file (JSON or TOML format)
 */
export class FileDataSource implements SuperpositionDataSource {
    private filePath: string;
    private nativeResolver: NativeResolver;

    constructor(filePath: string, nativeResolver?: NativeResolver) {
        this.filePath = filePath;
        this.nativeResolver = nativeResolver || new NativeResolver();
    }

    /**
     * Fetch the latest configuration from the data source
     */
    async fetchConfig(): Promise<ConfigData> {
        try {
            const content = await readFile(this.filePath, "utf-8");
            const ext = path.extname(this.filePath).toLowerCase();

            if (ext === ".toml") {
                // Use native TOML parser from superposition-bindings
                const parsed = this.nativeResolver.parseTomlConfig(content);
                return {
                    default_configs: parsed.default_configs || {},
                    contexts: parsed.contexts || [],
                    overrides: parsed.overrides || {},
                    dimensions: parsed.dimensions || {},
                    fetched_at: new Date(),
                };
            } else if (ext === ".json") {
                // Use JSON parser for JSON files
                const data = JSON.parse(content);
                return {
                    default_configs: data.default_configs || {},
                    contexts: data.contexts || [],
                    overrides: data.overrides || {},
                    dimensions: data.dimensions || {},
                    fetched_at: new Date(),
                };
            } else {
                // Try JSON first, then TOML
                try {
                    const data = JSON.parse(content);
                    return {
                        default_configs: data.default_configs || {},
                        contexts: data.contexts || [],
                        overrides: data.overrides || {},
                        dimensions: data.dimensions || {},
                        fetched_at: new Date(),
                    };
                } catch {
                    // Try TOML if JSON fails
                    const parsed = this.nativeResolver.parseTomlConfig(content);
                    return {
                        default_configs: parsed.default_configs || {},
                        contexts: parsed.contexts || [],
                        overrides: parsed.overrides || {},
                        dimensions: parsed.dimensions || {},
                        fetched_at: new Date(),
                    };
                }
            }
        } catch (error) {
            throw new Error(
                `Failed to read or parse config file at ${this.filePath}: ${error}`
            );
        }
    }

    /**
     * Fetch configuration with context/prefix filters
     */
    async fetchFilteredConfig(
        context?: Record<string, any>,
        prefixFilter?: string[]
    ): Promise<ConfigData> {
        const config = await this.fetchConfig();

        let filteredContexts = config.contexts;
        if (context && Object.keys(context).length > 0) {
            filteredContexts = config.contexts.filter((ctx) =>
                this.matchesContext(ctx, context)
            );
        }

        let filteredConfigs = config.default_configs;
        if (prefixFilter && prefixFilter.length > 0) {
            filteredConfigs = this.filterByPrefix(
                config.default_configs,
                prefixFilter
            );
        }

        return {
            default_configs: filteredConfigs,
            contexts: filteredContexts,
            overrides: config.overrides,
            dimensions: config.dimensions,
            fetched_at: config.fetched_at,
        };
    }

    /**
     * Fetch all active experiment data
     * File data source doesn't support experiments
     */
    async fetchActiveExperiments(): Promise<ExperimentData | null> {
        return null;
    }

    /**
     * Fetch active experiments filtered with partial context matching
     * File data source doesn't support experiments
     */
    async fetchCandidateActiveExperiments(
        _context?: Record<string, any>,
        _prefixFilter?: string[]
    ): Promise<ExperimentData | null> {
        return null;
    }

    /**
     * Fetch active experiments filtered with exact context matching
     * File data source doesn't support experiments
     */
    async fetchMatchingActiveExperiments(
        _context?: Record<string, any>,
        _prefixFilter?: string[]
    ): Promise<ExperimentData | null> {
        return null;
    }

    /**
     * Check if this data source supports experiments
     */
    supportsExperiments(): boolean {
        return false;
    }

    /**
     * Close and cleanup resources
     */
    async close(): Promise<void> {
        // No cleanup needed for file data source
    }

    /**
     * Check if context matches filter criteria
     */
    private matchesContext(
        context: Record<string, any>,
        filter: Record<string, any>
    ): boolean {
        for (const [key, value] of Object.entries(filter)) {
            if (context[key] !== value) {
                return false;
            }
        }
        return true;
    }

    /**
     * Filter config keys by prefixes
     */
    private filterByPrefix(
        configs: Record<string, any>,
        prefixes: string[]
    ): Record<string, any> {
        const filtered: Record<string, any> = {};

        for (const [key, value] of Object.entries(configs)) {
            if (prefixes.some((prefix) => key.startsWith(prefix))) {
                filtered[key] = value;
            }
        }

        return filtered;
    }
}
