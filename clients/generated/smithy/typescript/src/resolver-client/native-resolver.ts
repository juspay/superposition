import * as path from 'path';

export class NativeResolver {
    private lib: any;

    constructor(libPath?: string) {
        const koffi = require('koffi');
        this.lib = koffi.load(libPath || this.getDefaultLibPath());

        // Define only the core resolution functions
        this.lib.core_get_resolved_config = this.lib.func(
            'char* core_get_resolved_config(const char*, const char*, const char*, const char*, const char*)'
        );
        this.lib.core_get_resolved_config_with_reasoning = this.lib.func(
            'char* core_get_resolved_config_with_reasoning(const char*, const char*, const char*, const char*, const char*)'
        );
        this.lib.core_free_string = this.lib.func('void core_free_string(char*)');
        this.lib.core_last_error_message = this.lib.func('char* core_last_error_message()');
        this.lib.core_last_error_length = this.lib.func('int core_last_error_length()');
        this.lib.core_evaluate_experiments = this.lib.func(
            'char* core_evaluate_experiments(const char*, const char*, const char*, const char*, int, const char*)'
        );
        this.lib.core_test_connection = this.lib.func('int core_test_connection()');
    }

    resolveConfig(
        defaultConfigs: Record<string, any>,
        contexts: any[],
        overrides: Record<string, Record<string, any>>,
        queryData: Record<string, any>,
        mergeStrategy: 'merge' | 'replace' = 'merge'
    ): Record<string, any> {
        // if (!defaultConfigs) {
        //     throw new Error('defaultConfigs parameter is required');
        // }
        if (!contexts) {
            throw new Error('contexts parameter is required');
        }
        if (!overrides) {
            throw new Error('overrides parameter is required');
        }
        if (!queryData) {
            throw new Error('queryData parameter is required');
        }
        if (!mergeStrategy) {
            throw new Error('mergeStrategy parameter is required');
        }

        const defaultConfigsJson = JSON.stringify(defaultConfigs);
        const contextsJson = JSON.stringify(contexts);
        const overridesJson = JSON.stringify(overrides);
        const queryDataJson = JSON.stringify(queryData);

        console.log('🔧 Calling FFI with parameters:');
        // console.log('  defaultConfigs length:', defaultConfigsJson.length);
        console.log('  defaultConfigs:', defaultConfigs);
        console.log('  contexts length:', contextsJson.length);
        console.log('  overrides length:', overridesJson.length);
        console.log('  queryData length:', queryDataJson.length);
        console.log('  mergeStrategy:', mergeStrategy);

        // Validate JSON strings are not empty
        if (!defaultConfigsJson || defaultConfigsJson === 'null' || defaultConfigsJson === 'undefined') {
            throw new Error('defaultConfigs serialization failed');
        }
        if (!contextsJson || contextsJson === 'null' || contextsJson === 'undefined') {
            throw new Error('contexts serialization failed');
        }
        if (!overridesJson || overridesJson === 'null' || overridesJson === 'undefined') {
            throw new Error('overrides serialization failed');
        }
        if (!queryDataJson || queryDataJson === 'null' || queryDataJson === 'undefined') {
            throw new Error('queryData serialization failed');
        }



        const result = this.lib.core_get_resolved_config(
            defaultConfigsJson,
            contextsJson,
            overridesJson,
            queryDataJson,
            mergeStrategy
        );

        console.log('🔧 FFI call completed, result:', result);

        if (!result) {
            this.throwLastError('Failed to resolve config');
        }

        const configStr = typeof result === 'string' ? result : this.lib.decode(result, 'string');

        if (typeof result !== 'string') {
            this.lib.core_free_string(result);
        }

        return JSON.parse(configStr);
    }

    resolveConfigWithReasoning(
        defaultConfigs: Record<string, any>,
        contexts: any[],
        overrides: Record<string, Record<string, any>>,
        queryData: Record<string, any>,
        mergeStrategy: 'merge' | 'replace' = 'merge'
    ): Record<string, any> {
        const result = this.lib.core_get_resolved_config_with_reasoning(
            JSON.stringify(defaultConfigs),
            JSON.stringify(contexts),
            JSON.stringify(overrides),
            JSON.stringify(queryData),
            mergeStrategy
        );

        if (!result) {
            this.throwLastError('Failed to resolve config with reasoning');
        }

        const configStr = typeof result === 'string' ? result : this.lib.decode(result, 'string');

        if (typeof result !== 'string') {
            this.lib.core_free_string(result);
        }

        return JSON.parse(configStr);
    }

    evaluateExperiments(
        experiments: any[],
        variants: any[],
        overrides: Record<string, Record<string, any>>,
        userContext: Record<string, any>,
        toss: number,
        filterPrefixes: string[] = []
    ): Record<string, any> {
        if (!experiments) {
            throw new Error('experiments parameter is required');
        }
        if (!variants) {
            throw new Error('variants parameter is required');
        }
        if (!overrides) {
            throw new Error('overrides parameter is required');
        }
        if (!userContext) {
            throw new Error('userContext parameter is required');
        }

        const experimentsJson = JSON.stringify(experiments);
        const variantsJson = JSON.stringify(variants);
        const overridesJson = JSON.stringify(overrides);
        const userContextJson = JSON.stringify(userContext);
        const filterPrefixesJson = JSON.stringify(filterPrefixes);

        console.log('🔧 Calling experiment FFI with parameters:');
        console.log('  experiments length:', experimentsJson.length);
        console.log('  variants length:', variantsJson.length);
        console.log('  overrides length:', overridesJson.length);
        console.log('  userContext:', userContext);
        console.log('  toss:', toss);
        console.log('  filterPrefixes:', filterPrefixes);

        const result = this.lib.core_evaluate_experiments(
            experimentsJson,
            variantsJson,
            overridesJson,
            userContextJson,
            toss,
            filterPrefixesJson
        );

        console.log('🔧 FFI call completed, result:', result);

        if (!result) {
            this.throwLastError('Failed to evaluate experiments');
        }

        const resultStr = typeof result === 'string' ? result : this.lib.decode(result, 'string');

        if (typeof result !== 'string') {
            this.lib.core_free_string(result);
        }

        return JSON.parse(resultStr);
    }

    private getDefaultLibPath(): string {
        let filename;
        if (process.platform === 'win32') {
            filename = 'core.dll';
        } else if (process.platform === 'darwin') {
            filename = 'libcore.dylib';
        } else {
            filename = 'libcore.so';
        }

        return path.resolve(__dirname, `../../../../../../target/release/${filename}`);
    }

    private throwLastError(prefix: string): never {
        const errorLength = this.lib.core_last_error_length();
        if (errorLength > 0) {
            const errorPtr = this.lib.core_last_error_message();
            const errorMsg = typeof errorPtr === 'string' ? errorPtr : this.lib.decode(errorPtr, 'string');
            if (typeof errorPtr !== 'string') {
                this.lib.core_free_string(errorPtr);
            }
            throw new Error(`${prefix}: ${errorMsg}`);
        }
        throw new Error(`${prefix}: Unknown error`);
    }
}