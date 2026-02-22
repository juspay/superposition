import {
    SuperpositionDataSource,
    ConfigData,
    ExperimentData,
    SuperpositionOptions,
} from "../types";

/**
 * HTTP-based implementation of SuperpositionDataSource
 * Makes HTTP requests to fetch configuration and experiment data
 */
export class HttpDataSource implements SuperpositionDataSource {
    private options: SuperpositionOptions;

    constructor(options: SuperpositionOptions) {
        this.options = options;
    }

    /**
     * Fetch the latest configuration from the data source
     */
    async fetchConfig(): Promise<ConfigData> {
        const response = await this.makeRequest("/config");
        const data = await response.json();

        return {
            default_configs: data.default_configs || {},
            contexts: data.contexts || [],
            overrides: data.overrides || {},
            dimensions: data.dimensions || {},
            fetched_at: new Date(),
        };
    }

    /**
     * Fetch configuration with context/prefix filters
     */
    async fetchFilteredConfig(
        context?: Record<string, any>,
        prefixFilter?: string[]
    ): Promise<ConfigData> {
        const queryParams = new URLSearchParams();

        if (context && Object.keys(context).length > 0) {
            queryParams.set("context", JSON.stringify(context));
        }

        if (prefixFilter && prefixFilter.length > 0) {
            prefixFilter.forEach((prefix) => {
                queryParams.append("prefix", prefix);
            });
        }

        const queryString = queryParams.toString();
        const path = queryString ? `/config?${queryString}` : "/config";
        const response = await this.makeRequest(path);
        const data = await response.json();

        return {
            default_configs: data.default_configs || {},
            contexts: data.contexts || [],
            overrides: data.overrides || {},
            dimensions: data.dimensions || {},
            fetched_at: new Date(),
        };
    }

    /**
     * Fetch all active experiment data
     */
    async fetchActiveExperiments(): Promise<ExperimentData | null> {
        try {
            const [experimentsResponse, experimentGroupsResponse] = await Promise.all([
                this.makeRequest("/experiments?status=created&status=inprogress"),
                this.makeRequest("/experiment-groups"),
            ]);

            const experimentsData = await experimentsResponse.json();
            const experimentGroupsData = await experimentGroupsResponse.json();

            return {
                experiments: experimentsData.data || [],
                experiment_groups: experimentGroupsData.data || [],
                fetched_at: new Date(),
            };
        } catch (error) {
            console.error("Error fetching active experiments:", error);
            return null;
        }
    }

    /**
     * Fetch active experiments filtered with partial context matching
     */
    async fetchCandidateActiveExperiments(
        context?: Record<string, any>,
        prefixFilter?: string[]
    ): Promise<ExperimentData | null> {
        return this.fetchExperimentsWithFilters(context, prefixFilter, "partial");
    }

    /**
     * Fetch active experiments filtered with exact context matching
     */
    async fetchMatchingActiveExperiments(
        context?: Record<string, any>,
        prefixFilter?: string[]
    ): Promise<ExperimentData | null> {
        return this.fetchExperimentsWithFilters(context, prefixFilter, "exact");
    }

    /**
     * Check if this data source supports experiments
     */
    supportsExperiments(): boolean {
        return true;
    }

    /**
     * Close and cleanup resources
     */
    async close(): Promise<void> {
        // No cleanup needed for HTTP data source
    }

    /**
     * Make HTTP request with proper headers
     */
    private async makeRequest(path: string): Promise<Response> {
        const url = `${this.options.endpoint}${path}`;
        const headers: Record<string, string> = {
            "Content-Type": "application/json",
        };

        if (this.options.token) {
            headers["Authorization"] = `Bearer ${this.options.token}`;
        }

        if (this.options.org_id) {
            headers["x-org-id"] = this.options.org_id;
        }

        if (this.options.workspace_id) {
            headers["x-workspace"] = this.options.workspace_id;
        }

        let response: Response;

        if (this.options.httpClient) {
            response = await this.options.httpClient({
                url,
                headers,
                method: "GET",
            });
        } else {
            response = await fetch(url, {
                method: "GET",
                headers,
            });
        }

        if (!response.ok) {
            const errorText = await response.text();
            throw new Error(
                `HTTP ${response.status}: ${errorText || response.statusText}`
            );
        }

        return response;
    }

    /**
     * Fetch experiments with context and prefix filters
     */
    private async fetchExperimentsWithFilters(
        context?: Record<string, any>,
        prefixFilter?: string[],
        matchType?: "partial" | "exact"
    ): Promise<ExperimentData | null> {
        try {
            const queryParams = new URLSearchParams();
            queryParams.append("status", "created");
            queryParams.append("status", "inprogress");

            if (matchType) {
                queryParams.set("match_type", matchType);
            }

            if (context && Object.keys(context).length > 0) {
                queryParams.set("context", JSON.stringify(context));
            }

            if (prefixFilter && prefixFilter.length > 0) {
                prefixFilter.forEach((prefix) => {
                    queryParams.append("prefix", prefix);
                });
            }

            const experimentsPath = `/experiments?${queryParams.toString()}`;
            const experimentGroupsPath = "/experiment-groups";

            const [experimentsResponse, experimentGroupsResponse] = await Promise.all([
                this.makeRequest(experimentsPath),
                this.makeRequest(experimentGroupsPath),
            ]);

            const experimentsData = await experimentsResponse.json();
            const experimentGroupsData = await experimentGroupsResponse.json();

            return {
                experiments: experimentsData.data || [],
                experiment_groups: experimentGroupsData.data || [],
                fetched_at: new Date(),
            };
        } catch (error) {
            console.error("Error fetching filtered experiments:", error);
            return null;
        }
    }
}
