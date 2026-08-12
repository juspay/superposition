/**
 * SuperpositionAPIProvider — direct remote evaluation with no local caching.
 *
 * Mirrors Rust/Python/Java `SuperpositionAPIProvider`: every flag evaluation goes straight to the
 * Superposition API. Suitable for serverless / stateless deployments. Resolution is async-only.
 */

import {
    EvaluationContext,
    Provider,
    ProviderEvents,
    ProviderMetadata,
    ProviderStatus,
} from "@openfeature/server-sdk";

import {
    SuperpositionClient,
    GetResolvedConfigWithIdentifierCommand,
    ApplicableVariantsCommand,
} from "superposition-sdk";

import { AllFeatureProvider, FeatureExperimentMeta } from "./interfaces";
import { SuperpositionError } from "./errors";
import {
    SuperpositionOptions,
    sdkAuthConfig,
    validateOptions,
} from "./options";

export class SuperpositionAPIProvider
    extends AllFeatureProvider
    implements Provider, FeatureExperimentMeta
{
    readonly metadata: ProviderMetadata = { name: "SuperpositionAPIProvider" };
    status: ProviderStatus = ProviderStatus.NOT_READY;

    private globalContext: EvaluationContext = {};
    private client: SuperpositionClient | null;

    constructor(private readonly options: SuperpositionOptions) {
        super();
        validateOptions(options);
        this.client = new SuperpositionClient({
            endpoint: options.endpoint,
            ...sdkAuthConfig(options.auth),
        });
    }

    async initialize(context?: EvaluationContext): Promise<void> {
        this.globalContext = context ?? {};
        this.status = ProviderStatus.READY;
        this.events.emit(ProviderEvents.Ready, { message: "Provider ready" });
    }

    async onClose(): Promise<void> {
        return this.shutdown();
    }

    async shutdown(): Promise<void> {
        if (this.client) {
            if (typeof (this.client as any).destroy === "function") {
                (this.client as any).destroy();
            }
            this.client = null;
        }
        this.status = ProviderStatus.NOT_READY;
    }

    async resolveAllFeaturesWithFilter(
        context: EvaluationContext,
        prefixFilter?: string[],
        excludePrefixFilter?: string[],
    ): Promise<Record<string, any>> {
        const [targetingKey, queryData] = this.mergeContexts(context);
        const response = await this.requireClient().send(
            new GetResolvedConfigWithIdentifierCommand({
                workspace_id: this.options.workspaceId,
                org_id: this.options.orgId,
                context: queryData,
                prefix: prefixFilter,
                exclude_prefix: excludePrefixFilter,
                identifier: targetingKey,
            }),
        );

        const config = response.config;
        if (config && typeof config === "object" && !Array.isArray(config)) {
            return config as Record<string, any>;
        }
        // Wrap non-object responses so the type-coercion contract still has a map to look flags up in.
        return { _value: config };
    }

    async getApplicableVariants(
        context: EvaluationContext,
        prefixFilter?: string[],
        excludePrefixFilter?: string[],
    ): Promise<string[]> {
        const [targetingKey, queryData] = this.mergeContexts(context);
        // Guard outside the try so a post-shutdown PROVIDER_ERROR propagates rather than being
        // swallowed to [] by the catch below (which only exists to tolerate genuine API errors).
        const client = this.requireClient();
        try {
            const response = await client.send(
                new ApplicableVariantsCommand({
                    workspace_id: this.options.workspaceId,
                    org_id: this.options.orgId,
                    identifier: targetingKey ?? "",
                    context: queryData,
                    prefix: prefixFilter,
                    exclude_prefix: excludePrefixFilter,
                }),
            );
            return (response.data ?? []).map((v: any) => v.id).filter(Boolean);
        } catch (e) {
            throw SuperpositionError.networkError(
                `Failed to get applicable variants: ${e instanceof Error ? e.message : String(e)}`,
                e,
            );
        }
    }

    private requireClient(): SuperpositionClient {
        if (!this.client) {
            throw SuperpositionError.providerError(
                "SuperpositionAPIProvider is shut down",
            );
        }
        return this.client;
    }

    private mergeContexts(
        context?: EvaluationContext,
    ): [string | undefined, Record<string, any>] {
        const merged: EvaluationContext = {
            ...this.globalContext,
            ...(context ?? {}),
        };
        const { targetingKey, ...attributes } = merged;
        return [targetingKey, attributes as Record<string, any>];
    }
}
