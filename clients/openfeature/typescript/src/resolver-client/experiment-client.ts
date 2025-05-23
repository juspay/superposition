import { NativeResolver } from './native-resolver';

export interface ExperimentVariant {
    id: string;
    variant_type: 'CONTROL' | 'EXPERIMENTAL';
    context_id?: string;
    overrides: Record<string, any>;
}

export interface ExperimentResponse {
    id: string;
    name: string;
    context: Record<string, any>;
    traffic_percentage: number;
    override_keys: string[];
    variants: ExperimentVariant[];
    status: string;
    created_by: string;
    created_at: string;
    last_modified: string;
    last_modified_by: string;
}

export interface EvaluationOptions {
    context_eval?: 'Full' | 'Partial';
    variant_selection?: 'Simple' | 'Bucketed' | 'Experimental';
    include_reasoning?: boolean;
}

export interface VariantReasoning {
    variant_id: string;
    variant_type: 'CONTROL' | 'EXPERIMENTAL';
    context_matched: boolean;
    selected: boolean;
}

export interface ExperimentReasoning {
    experiment_id: string;
    experiment_name: string;
    context_matched: boolean;
    traffic_passed: boolean;
    variants_evaluated: VariantReasoning[];
}

export interface ExperimentResult {
    variants: ExperimentVariant[];
    overrides: Record<string, any>;
    experiments_evaluated: number;
    reasoning?: ExperimentReasoning[];
}

export interface ExperimentContext {
    __experiment_overrides?: Record<string, any>;
    __experiment_variants?: ExperimentVariant[];
    __experiment_enabled?: boolean;
    [key: string]: any;
}

export class ExperimentationClient {
    private resolver: NativeResolver;
    private experiments: ExperimentResponse[] = [];
    private variants: ExperimentVariant[] = [];
    private overrides: Record<string, Record<string, any>> = {};
    private defaultToss: number = 50;
    private filterPrefixes: string[] = [];

    constructor(
        resolver: NativeResolver,
    ) {
        this.resolver = resolver;
    }

    setExperimentData(
        experiments: ExperimentResponse[],
        variants: ExperimentVariant[],
        overrides: Record<string, Record<string, any>>
    ): void {
        this.experiments = experiments;
        this.variants = variants;
        this.overrides = overrides;
    }

    getApplicableVariants(
        context: Record<string, any>,
        toss: number,
        options?: EvaluationOptions
    ): string[] {
        const result = this.evaluateExperiments(context, toss, options);
        return result.variants.map(v => v.id);
    }

    getSatisfiedExperiments(
        context: Record<string, any>,
        filterPrefixes?: string[]
    ): ExperimentResponse[] {
        const prefixes = filterPrefixes ?? this.filterPrefixes;
        const userContextValue = context;

        return this.experiments.filter(experiment => {
            if (prefixes.length > 0) {
                const matchesPrefix = prefixes.some(prefix =>
                    experiment.name.startsWith(prefix) || experiment.id.startsWith(prefix)
                );
                if (!matchesPrefix) return false;
            }

            try {
                return this.evaluateContext(experiment.context, userContextValue);
            } catch {
                return false;
            }
        });
    }

    evaluateExperiments(
        context: Record<string, any>,
        toss: number,
        options?: EvaluationOptions
    ): ExperimentResult {

        try {
            const result = this.resolver.evaluateExperiments(
                this.experiments,
                this.variants,
                this.overrides,
                context,
                toss,
                this.filterPrefixes,
                options
            );

            return result as ExperimentResult;
        } catch (error) {
            console.error('Experiment evaluation failed:', error);
            return {
                variants: [],
                overrides: {},
                experiments_evaluated: 0,
                reasoning: options?.include_reasoning ? [] : undefined
            };
        }
    }

    getExperimentationContext(
        baseContext: Record<string, any>,
        toss: number,
        options?: EvaluationOptions
    ): ExperimentContext {
        const result = this.evaluateExperiments(baseContext, toss, options);

        const experimentContext: ExperimentContext = {
            ...baseContext,
            __experiment_enabled: true,
            __experiment_overrides: result.overrides,
            __experiment_variants: result.variants
        };

        return experimentContext;
    }

    hasRunningExperiments(context: Record<string, any>): boolean {
        const satisfied = this.getSatisfiedExperiments(context);
        return satisfied.some(exp => exp.status === 'INPROGRESS');
    }

    getExperimentVariants(experimentId: string): ExperimentVariant[] {
        return this.variants.filter(v => v.id === experimentId);
    }

    private evaluateContext(
        experimentContext: Record<string, any>,
        userContext: Record<string, any>
    ): boolean {
        try {
            for (const [key, value] of Object.entries(experimentContext)) {
                if (key in userContext) {
                    if (Array.isArray(value)) {
                        if (!value.includes(userContext[key])) {
                            return false;
                        }
                    } else if (userContext[key] !== value) {
                        return false;
                    }
                }
            }
            return true;
        } catch {
            return false;
        }
    }

    async refreshCache(): Promise<void> {
        console.log('Refreshing experiment cache...');

        try {
            console.log('Experiment cache refresh completed (placeholder implementation)');
        } catch (error) {
            console.error('Failed to refresh experiment cache:', error);
        }
    }

}