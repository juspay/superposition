import path from "path";
import os from "os";
import fs from "fs";
import koffi from "koffi";
import { fileURLToPath } from "url";

export class NativeResolver {
    private lib: any;
    private isAvailable: boolean = false;

    constructor(libPath?: string) {
        try {
            this.lib = koffi.load(libPath || this.getDefaultLibPath());

            // Define the core resolution functions with CORRECT 8 parameters each
            this.lib.core_get_resolved_config = this.lib.func(
                "char* core_get_resolved_config(const char*, const char*, const char*, const char*, const char*, const char*, const char*, const char*)"
            );
            this.lib.core_get_resolved_config_with_reasoning = this.lib.func(
                "char* core_get_resolved_config_with_reasoning(const char*, const char*, const char*, const char*, const char*, const char*, const char*, const char*)"
            );
            this.lib.core_free_string = this.lib.func(
                "void core_free_string(char*)"
            );
            this.lib.core_last_error_message = this.lib.func(
                "char* core_last_error_message()"
            );
            this.lib.core_last_error_length = this.lib.func(
                "int core_last_error_length()"
            );
            this.lib.core_get_applicable_variants = this.lib.func(
                "char* core_get_applicable_variants(const char*, const char*, const char*, const char*)"
            );
            this.lib.core_test_connection = this.lib.func(
                "int core_test_connection()"
            );

            this.isAvailable = true;
        } catch (error) {
            console.warn(
                "Native resolver library not available, falling back to JavaScript implementation:",
                error
            );
            this.isAvailable = false;
        }
    }

    isNativeAvailable(): boolean {
        return this.isAvailable;
    }

    resolveConfig(
        defaultConfigs: Record<string, any>,
        contexts: any[],
        overrides: Record<string, Record<string, any>>,
        dimensions: Record<string, Record<string, any>>,
        queryData: Record<string, any>,
        mergeStrategy: "merge" | "replace" = "merge",
        filterPrefixes?: string[],
        experimentation?: any
    ): Record<string, any> {
        if (!this.isAvailable) {
            throw new Error(
                "Native resolver is not available. Please ensure the native library is built and accessible."
            );
        }

        // Input validation
        if (!contexts) {
            throw new Error("contexts parameter is required");
        }
        if (!overrides) {
            throw new Error("overrides parameter is required");
        }
        if (!dimensions) {
            throw new Error("dimensions parameter is required");
        }
        if (!queryData) {
            throw new Error("queryData parameter is required");
        }
        if (!mergeStrategy) {
            throw new Error("mergeStrategy parameter is required");
        }

        const defaultConfigsJson = JSON.stringify(defaultConfigs || {});
        const contextsJson = JSON.stringify(contexts);
        const overridesJson = JSON.stringify(overrides);
        const dimensionsJson = JSON.stringify(dimensions);
        const queryDataJson = JSON.stringify(queryData);
        const filterPrefixesJson =
            filterPrefixes && filterPrefixes.length > 0
                ? JSON.stringify(filterPrefixes)
                : null;
        const experimentationJson = experimentation
            ? JSON.stringify(experimentation)
            : null;

        console.log("🔧 Calling FFI with parameters:");
        console.log("  defaultConfigs:", defaultConfigs);
        console.log("  contexts length:", contextsJson.length);
        console.log("  overrides length:", overridesJson.length);
        console.log("  dimensions length:", dimensionsJson.length);
        console.log("  queryData :", queryDataJson);
        console.log("  mergeStrategy:", mergeStrategy);
        console.log("  filterPrefixes:", filterPrefixes);
        console.log("  experimentation:", experimentationJson);

        if (
            !defaultConfigsJson ||
            defaultConfigsJson === "null" ||
            defaultConfigsJson === "undefined"
        ) {
            throw new Error("defaultConfigs serialization failed");
        }
        if (
            !contextsJson ||
            contextsJson === "null" ||
            contextsJson === "undefined"
        ) {
            throw new Error("contexts serialization failed");
        }
        if (
            !overridesJson ||
            overridesJson === "null" ||
            overridesJson === "undefined"
        ) {
            throw new Error("overrides serialization failed");
        }
        if (
            !dimensionsJson ||
            dimensionsJson === "null" ||
            dimensionsJson === "undefined"
        ) {
            throw new Error("dimensions serialization failed");
        }
        if (
            !queryDataJson ||
            queryDataJson === "null" ||
            queryDataJson === "undefined"
        ) {
            throw new Error("queryData serialization failed");
        }

        const result = this.lib.core_get_resolved_config(
            defaultConfigsJson,
            contextsJson,
            overridesJson,
            dimensionsJson,
            queryDataJson,
            mergeStrategy,
            filterPrefixesJson,
            experimentationJson
        );

        console.log("🔧 FFI call completed, result:", result);

        if (!result) {
            this.throwLastError("Failed to resolve config");
        }

        const configStr =
            typeof result === "string"
                ? result
                : this.lib.decode(result, "string");

        if (typeof result !== "string") {
            this.lib.core_free_string(result);
        }

        try {
            return JSON.parse(configStr);
        } catch (parseError) {
            console.error("Failed to parse config result:", parseError);
            console.error("Raw result string:", configStr);
            throw new Error(
                `Failed to parse config evaluation result: ${parseError}`
            );
        }
    }

    resolveConfigWithReasoning(
        defaultConfigs: Record<string, any>,
        contexts: any[],
        overrides: Record<string, Record<string, any>>,
        dimensions: Record<string, Record<string, any>>,
        queryData: Record<string, any>,
        mergeStrategy: "merge" | "replace" = "merge",
        filterPrefixes?: string[],
        experimentation?: any
    ): Record<string, any> {
        if (!this.isAvailable) {
            throw new Error(
                "Native resolver is not available. Please ensure the native library is built and accessible."
            );
        }

        const filterPrefixesJson =
            filterPrefixes && filterPrefixes.length > 0
                ? JSON.stringify(filterPrefixes)
                : null;
        const experimentationJson = experimentation
            ? JSON.stringify(experimentation)
            : null;

        const result = this.lib.core_get_resolved_config_with_reasoning(
            JSON.stringify(defaultConfigs || {}),
            JSON.stringify(contexts),
            JSON.stringify(overrides),
            JSON.stringify(dimensions),
            JSON.stringify(queryData),
            mergeStrategy,
            filterPrefixesJson,
            experimentationJson
        );

        if (!result) {
            this.throwLastError("Failed to resolve config with reasoning");
        }

        const configStr =
            typeof result === "string"
                ? result
                : this.lib.decode(result, "string");

        if (typeof result !== "string") {
            this.lib.core_free_string(result);
        }

        try {
            return JSON.parse(configStr);
        } catch (parseError) {
            console.error("Failed to parse reasoning result:", parseError);
            console.error("Raw result string:", configStr);
            throw new Error(
                `Failed to parse reasoning evaluation result: ${parseError}`
            );
        }
    }

    getApplicableVariants(
        experiments: any[],
        userContext: Record<string, any>,
        identifier: string,
        filterPrefixes: string[] = []
    ): string[] {
        if (!this.isAvailable) {
            throw new Error(
                "Native resolver is not available. Please ensure the native library is built and accessible."
            );
        }

        if (!experiments) {
            throw new Error("experiments parameter is required");
        }
        if (!userContext) {
            throw new Error("userContext parameter is required");
        }

        const experimentsJson = JSON.stringify(experiments);
        const userContextJson = JSON.stringify(userContext);
        const filterPrefixesJson =
            filterPrefixes.length > 0 ? JSON.stringify(filterPrefixes) : null;

        console.log("🔧 Calling FFI getApplicableVariants with parameters:");
        console.log("  experiments:", experiments);
        console.log("  userContext:", userContext);
        console.log("  identifier:", identifier);
        console.log("  filterPrefixes:", filterPrefixes);

        const result = this.lib.core_get_applicable_variants(
            experimentsJson,
            userContextJson,
            identifier,
            filterPrefixesJson
        );

        console.log(
            "FFI getApplicableVariants call completed, result:",
            result
        );

        if (!result) {
            this.throwLastError("Failed to get applicable variants");
        }

        const resultStr =
            typeof result === "string"
                ? result
                : this.lib.decode(result, "string");

        if (typeof result !== "string") {
            this.lib.core_free_string(result);
        }

        try {
            return JSON.parse(resultStr);
        } catch (parseError) {
            console.error("Failed to parse variants result:", parseError);
            console.error("Raw result string:", resultStr);
            throw new Error(
                `Failed to parse variants evaluation result: ${parseError}`
            );
        }
    }

    /**
     * Get the path to the native library.
     * Uses the same approach as Java and Python - looks for GitHub artifacts first,
     * then falls back to local build.
     */
    private getDefaultLibPath(): string {
        const platform = os.platform();
        const arch = os.arch();

        let filename: string;
        let extension: string;

        // Determine file extension based on platform
        if (platform === "win32" && arch === "x64") {
            extension = "x86_64-pc-windows-msvc.dll";
        } else if (platform === "darwin" && arch === "arm64") {
            extension = "aarch64-apple-darwin.dylib";
        } else if (platform === "darwin" && arch === "x64") {
            extension = "x86_64-apple-darwin.dylib";
        } else {
            extension = "x86_64-unknown-linux-gnu.so";
        }

        filename = `libsuperposition_core-${extension}`;

        const dirname = path.dirname(fileURLToPath(import.meta.url));

        const packageRootPath = path.resolve(dirname, "..", filename);
        if (this.fileExists(packageRootPath)) {
            console.log(
                `Using native library from package root: ${packageRootPath}`
            );
            return packageRootPath;
        }

        // 1. First try to load from package's native-lib directory (GitHub artifacts)
        const packageNativeLibPath = path.resolve(
            dirname,
            "native-lib",
            filename
        );
        if (this.fileExists(packageNativeLibPath)) {
            console.log(
                `Using native library from package: ${packageNativeLibPath}`
            );
            return packageNativeLibPath;
        }

        const packageNative2LibPath = path.resolve(
            dirname,
            "..",
            "native-lib",
            filename
        );
        if (this.fileExists(packageNative2LibPath)) {
            console.log(
                `Using native library from package: ${packageNative2LibPath}`
            );
            return packageNative2LibPath;
        }

        // 2. Try platform-specific subdirectory in native-lib
        const platformDir = `${platform}-${arch}`;
        const platformSpecificPath = path.resolve(
            dirname,
            "..",
            "native-lib",
            platformDir,
            filename
        );
        if (this.fileExists(platformSpecificPath)) {
            console.log(
                `Using platform-specific native library: ${platformSpecificPath}`
            );
            return platformSpecificPath;
        }

        // 3. Fall back to local build (relative to repository root)
        const localBuildPath = path.resolve(
            dirname,
            "..",
            "..",
            "..",
            "..",
            "target",
            "release",
            filename
        );
        if (this.fileExists(localBuildPath)) {
            console.log(`Using local build: ${localBuildPath}`);
            return localBuildPath;
        }

        // 4. Final fallback - assume it's in the system path
        console.warn(
            `Native library not found in expected locations, trying: ${filename}`
        );
        return filename;
    }

    private fileExists(filePath: string): boolean {
        try {
            return fs.existsSync(filePath);
        } catch {
            console.trace(`Binary not found for path ${filePath}`);
            return false;
        }
    }

    private throwLastError(prefix: string): never {
        if (!this.isAvailable) {
            throw new Error(`${prefix}: Native resolver not available`);
        }

        const errorLength = this.lib.core_last_error_length();
        if (errorLength > 0) {
            const errorPtr = this.lib.core_last_error_message();
            const errorMsg =
                typeof errorPtr === "string"
                    ? errorPtr
                    : this.lib.decode(errorPtr, "string");
            if (typeof errorPtr !== "string") {
                this.lib.core_free_string(errorPtr);
            }
            throw new Error(`${prefix}: ${errorMsg}`);
        }
        throw new Error(`${prefix}: Unknown error`);
    }
}
