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
    }

    resolveConfig(
        defaultConfigs: Record<string, any>,
        contexts: any[],
        overrides: Record<string, Record<string, any>>,
        queryData: Record<string, any>,
        mergeStrategy: 'merge' | 'replace' = 'merge'
    ): Record<string, any> {
        const result = this.lib.core_get_resolved_config(
            JSON.stringify(defaultConfigs),
            JSON.stringify(contexts),
            JSON.stringify(overrides),
            JSON.stringify(queryData),
            mergeStrategy
        );

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