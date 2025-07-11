export declare class NativeResolver {
    private lib;
    private isAvailable;
    constructor(libPath?: string);
    isNativeAvailable(): boolean;
    resolveConfig(defaultConfigs: Record<string, any>, contexts: any[], overrides: Record<string, Record<string, any>>, queryData: Record<string, any>, mergeStrategy?: 'merge' | 'replace', filterPrefixes?: string[], experimentation?: any): Record<string, any>;
    resolveConfigWithReasoning(defaultConfigs: Record<string, any>, contexts: any[], overrides: Record<string, Record<string, any>>, queryData: Record<string, any>, mergeStrategy?: 'merge' | 'replace', filterPrefixes?: string[], experimentation?: any): Record<string, any>;
    getApplicableVariants(experiments: any[], userContext: Record<string, any>, toss: number, filterPrefixes?: string[]): string[];
    /**
     * Get the path to the native library.
     * Uses the same approach as Java and Python - looks for GitHub artifacts first,
     * then falls back to local build.
     */
    private getDefaultLibPath;
    private fileExists;
    private throwLastError;
}
