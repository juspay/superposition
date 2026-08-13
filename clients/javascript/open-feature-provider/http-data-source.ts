/**
 * HTTP data source — fetches configuration and experiment data from the Superposition API.
 *
 * Mirrors Rust/Python/Java `HttpDataSource`. Supports conditional requests via `if_modified_since`:
 * the SDK documents that a 304 is surfaced as an error whose response code must be inspected, so a
 * 304 is detected by checking the thrown error's HTTP status — the JS analog of Python's
 * `_NotModifiedInterceptor` / Java's `ClientInterceptor`.
 */

import {
    SuperpositionClient,
    GetConfigCommand,
    GetExperimentConfigCommand,
    DimensionMatchStrategy,
} from "superposition-sdk";

import { SuperpositionOptions, sdkAuthConfig, validateOptions } from "./options";
import { SuperpositionError } from "./errors";
import {
    configResponseToFfiConfig,
    experimentConfigFromResponses,
} from "./conversions";
import {
    BaseDataSource,
    ConfigData,
    ExperimentData,
    FetchResponse,
} from "./data-source";

/** Whether a thrown SDK error represents an HTTP 304 Not Modified. */
function isNotModified(error: any): boolean {
    const status =
        error?.$metadata?.httpStatusCode ??
        error?.$response?.statusCode ??
        error?.statusCode;
    return status === 304;
}

export class HttpDataSource extends BaseDataSource {
    private client: SuperpositionClient | null;

    constructor(private readonly options: SuperpositionOptions) {
        super();
        validateOptions(options);
        this.client = this.createClient();
    }

    private createClient(): SuperpositionClient {
        return new SuperpositionClient({
            endpoint: this.options.endpoint,
            ...sdkAuthConfig(this.options.auth),
        });
    }

    private requireClient(): SuperpositionClient {
        if (!this.client) {
            throw SuperpositionError.dataSourceError("HTTP data source is closed");
        }
        return this.client;
    }

    async fetchFilteredConfig(
        context?: Record<string, any>,
        prefixFilter?: string[],
        excludePrefixFilter?: string[],
        ifModifiedSince?: Date
    ): Promise<FetchResponse<ConfigData>> {
        try {
            const response = await this.requireClient().send(
                new GetConfigCommand({
                    workspace_id: this.options.workspaceId,
                    org_id: this.options.orgId,
                    context,
                    prefix: prefixFilter?.length ? prefixFilter : undefined,
                    exclude_prefix: excludePrefixFilter?.length ? excludePrefixFilter : undefined,
                    if_modified_since: ifModifiedSince,
                })
            );
            return FetchResponse.data<ConfigData>({
                fetchedAt: response.last_modified ?? new Date(),
                data: configResponseToFfiConfig(response),
            });
        } catch (error) {
            if (isNotModified(error)) {
                return FetchResponse.notModified<ConfigData>();
            }
            throw SuperpositionError.networkError(
                `Failed to fetch config: ${error instanceof Error ? error.message : String(error)}`,
                error
            );
        }
    }

    private async fetchFilteredExperiment(
        context?: Record<string, any>,
        prefixFilter?: string[],
        excludePrefixFilter?: string[],
        ifModifiedSince?: Date,
        dimensionMatchStrategy?: DimensionMatchStrategy
    ): Promise<FetchResponse<ExperimentData>> {
        try {
            const response = await this.requireClient().send(
                new GetExperimentConfigCommand({
                    workspace_id: this.options.workspaceId,
                    org_id: this.options.orgId,
                    context,
                    prefix: prefixFilter?.length ? prefixFilter : undefined,
                    exclude_prefix: excludePrefixFilter?.length ? excludePrefixFilter : undefined,
                    if_modified_since: ifModifiedSince,
                    dimension_match_strategy: dimensionMatchStrategy,
                })
            );
            return FetchResponse.data<ExperimentData>({
                fetchedAt: response.last_modified ?? new Date(),
                data: experimentConfigFromResponses(
                    response.experiments,
                    response.experiment_groups
                ),
            });
        } catch (error) {
            if (isNotModified(error)) {
                return FetchResponse.notModified<ExperimentData>();
            }
            throw SuperpositionError.networkError(
                `Failed to fetch experiments: ${
                    error instanceof Error ? error.message : String(error)
                }`,
                error
            );
        }
    }

    fetchActiveExperiments(
        ifModifiedSince?: Date
    ): Promise<FetchResponse<ExperimentData>> {
        return this.fetchFilteredExperiment(undefined, undefined, undefined, ifModifiedSince);
    }

    fetchCandidateActiveExperiments(
        context?: Record<string, any>,
        prefixFilter?: string[],
        excludePrefixFilter?: string[],
        ifModifiedSince?: Date
    ): Promise<FetchResponse<ExperimentData>> {
        return this.fetchFilteredExperiment(
            context,
            prefixFilter,
            excludePrefixFilter,
            ifModifiedSince,
            DimensionMatchStrategy.EXACT
        );
    }

    fetchMatchingActiveExperiments(
        context?: Record<string, any>,
        prefixFilter?: string[],
        excludePrefixFilter?: string[],
        ifModifiedSince?: Date
    ): Promise<FetchResponse<ExperimentData>> {
        return this.fetchFilteredExperiment(
            context,
            prefixFilter,
            excludePrefixFilter,
            ifModifiedSince,
            DimensionMatchStrategy.SUBSET
        );
    }

    override supportsExperiments(): boolean {
        return true;
    }

    async close(): Promise<void> {
        if (this.client) {
            // Smithy clients expose destroy(); guard in case a custom handler doesn't.
            if (typeof (this.client as any).destroy === "function") {
                (this.client as any).destroy();
            }
            this.client = null;
        }
    }
}
