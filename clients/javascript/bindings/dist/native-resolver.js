"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null) for (var k in mod) if (k !== "default" && Object.prototype.hasOwnProperty.call(mod, k)) __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
};
Object.defineProperty(exports, "__esModule", { value: true });
exports.NativeResolver = void 0;
const path = __importStar(require("path"));
const os = __importStar(require("os"));
class NativeResolver {
    constructor(libPath) {
        this.isAvailable = false;
        try {
            const koffi = require('koffi');
            this.lib = koffi.load(libPath || this.getDefaultLibPath());
            // Define the core resolution functions with CORRECT 7 parameters each
            this.lib.core_get_resolved_config = this.lib.func('char* core_get_resolved_config(const char*, const char*, const char*, const char*, const char*, const char*, const char*)');
            this.lib.core_get_resolved_config_with_reasoning = this.lib.func('char* core_get_resolved_config_with_reasoning(const char*, const char*, const char*, const char*, const char*, const char*, const char*)');
            this.lib.core_free_string = this.lib.func('void core_free_string(char*)');
            this.lib.core_last_error_message = this.lib.func('char* core_last_error_message()');
            this.lib.core_last_error_length = this.lib.func('int core_last_error_length()');
            this.lib.core_get_applicable_variants = this.lib.func('char* core_get_applicable_variants(const char*, const char*, int8, const char*)');
            this.lib.core_test_connection = this.lib.func('int core_test_connection()');
            this.isAvailable = true;
        }
        catch (error) {
            console.warn('Native resolver library not available, falling back to JavaScript implementation:', error);
            this.isAvailable = false;
        }
    }
    isNativeAvailable() {
        return this.isAvailable;
    }
    resolveConfig(defaultConfigs, contexts, overrides, queryData, mergeStrategy = 'merge', filterPrefixes, experimentation) {
        if (!this.isAvailable) {
            throw new Error('Native resolver is not available. Please ensure the native library is built and accessible.');
        }
        // Input validation
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
        const defaultConfigsJson = JSON.stringify(defaultConfigs || {});
        const contextsJson = JSON.stringify(contexts);
        const overridesJson = JSON.stringify(overrides);
        const queryDataJson = JSON.stringify(queryData);
        const filterPrefixesJson = filterPrefixes && filterPrefixes.length > 0
            ? JSON.stringify(filterPrefixes)
            : null;
        const experimentationJson = experimentation ? JSON.stringify(experimentation) : null;
        console.log('🔧 Calling FFI with parameters:');
        console.log('  defaultConfigs:', defaultConfigs);
        console.log('  contexts length:', contextsJson.length);
        console.log('  overrides length:', overridesJson.length);
        console.log('  queryData :', queryDataJson);
        console.log('  mergeStrategy:', mergeStrategy);
        console.log('  filterPrefixes:', filterPrefixes);
        console.log('  experimentation:', experimentationJson);
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
        const result = this.lib.core_get_resolved_config(defaultConfigsJson, contextsJson, overridesJson, queryDataJson, mergeStrategy, filterPrefixesJson, experimentationJson);
        console.log('🔧 FFI call completed, result:', result);
        if (!result) {
            this.throwLastError('Failed to resolve config');
        }
        const configStr = typeof result === 'string' ? result : this.lib.decode(result, 'string');
        if (typeof result !== 'string') {
            this.lib.core_free_string(result);
        }
        try {
            return JSON.parse(configStr);
        }
        catch (parseError) {
            console.error('Failed to parse config result:', parseError);
            console.error('Raw result string:', configStr);
            throw new Error(`Failed to parse config evaluation result: ${parseError}`);
        }
    }
    resolveConfigWithReasoning(defaultConfigs, contexts, overrides, queryData, mergeStrategy = 'merge', filterPrefixes, experimentation) {
        if (!this.isAvailable) {
            throw new Error('Native resolver is not available. Please ensure the native library is built and accessible.');
        }
        const filterPrefixesJson = filterPrefixes && filterPrefixes.length > 0
            ? JSON.stringify(filterPrefixes)
            : null;
        const experimentationJson = experimentation
            ? JSON.stringify(experimentation)
            : null;
        const result = this.lib.core_get_resolved_config_with_reasoning(JSON.stringify(defaultConfigs || {}), JSON.stringify(contexts), JSON.stringify(overrides), JSON.stringify(queryData), mergeStrategy, filterPrefixesJson, experimentationJson);
        if (!result) {
            this.throwLastError('Failed to resolve config with reasoning');
        }
        const configStr = typeof result === 'string' ? result : this.lib.decode(result, 'string');
        if (typeof result !== 'string') {
            this.lib.core_free_string(result);
        }
        try {
            return JSON.parse(configStr);
        }
        catch (parseError) {
            console.error('Failed to parse reasoning result:', parseError);
            console.error('Raw result string:', configStr);
            throw new Error(`Failed to parse reasoning evaluation result: ${parseError}`);
        }
    }
    getApplicableVariants(experiments, userContext, toss, filterPrefixes = []) {
        if (!this.isAvailable) {
            throw new Error('Native resolver is not available. Please ensure the native library is built and accessible.');
        }
        if (!experiments) {
            throw new Error('experiments parameter is required');
        }
        if (!userContext) {
            throw new Error('userContext parameter is required');
        }
        const experimentsJson = JSON.stringify(experiments);
        const userContextJson = JSON.stringify(userContext);
        const filterPrefixesJson = filterPrefixes.length > 0 ? JSON.stringify(filterPrefixes) : null;
        console.log('🔧 Calling FFI getApplicableVariants with parameters:');
        console.log('  experiments:', experiments);
        console.log('  userContext:', userContext);
        console.log('  toss:', toss);
        console.log('  filterPrefixes:', filterPrefixes);
        const result = this.lib.core_get_applicable_variants(experimentsJson, userContextJson, toss, filterPrefixesJson);
        console.log('FFI getApplicableVariants call completed, result:', result);
        if (!result) {
            this.throwLastError('Failed to get applicable variants');
        }
        const resultStr = typeof result === 'string' ? result : this.lib.decode(result, 'string');
        if (typeof result !== 'string') {
            this.lib.core_free_string(result);
        }
        try {
            return JSON.parse(resultStr);
        }
        catch (parseError) {
            console.error('Failed to parse variants result:', parseError);
            console.error('Raw result string:', resultStr);
            throw new Error(`Failed to parse variants evaluation result: ${parseError}`);
        }
    }
    /**
     * Get the path to the native library.
     * Uses the same approach as Java and Python - looks for GitHub artifacts first,
     * then falls back to local build.
     */
    getDefaultLibPath() {
        const platform = os.platform();
        const arch = os.arch();
        let filename;
        let extension;
        // Determine file extension based on platform
        if (platform === 'win32') {
            extension = '.dll';
        }
        else if (platform === 'darwin') {
            extension = '.dylib';
        }
        else {
            extension = '.so';
        }
        // Create platform-specific filename
        if (platform === 'darwin') {
            filename = `libsuperposition_core${extension}`;
        }
        else {
            filename = `libcore${extension}`;
        }
        const packageRootPath = path.resolve(__dirname, '..', filename);
        if (this.fileExists(packageRootPath)) {
            console.log(`Using native library from package root: ${packageRootPath}`);
            return packageRootPath;
        }
        // 1. First try to load from package's native-lib directory (GitHub artifacts)
        const packageNativeLibPath = path.resolve(__dirname, '..', 'native-lib', filename);
        if (this.fileExists(packageNativeLibPath)) {
            console.log(`Using native library from package: ${packageNativeLibPath}`);
            return packageNativeLibPath;
        }
        // 2. Try platform-specific subdirectory in native-lib
        const platformDir = `${platform}-${arch}`;
        const platformSpecificPath = path.resolve(__dirname, '..', 'native-lib', platformDir, filename);
        if (this.fileExists(platformSpecificPath)) {
            console.log(`Using platform-specific native library: ${platformSpecificPath}`);
            return platformSpecificPath;
        }
        // 3. Fall back to local build (relative to repository root)
        const localBuildPath = path.resolve(__dirname, '..', '..', '..', '..', 'target', 'release', filename);
        if (this.fileExists(localBuildPath)) {
            console.log(`Using local build: ${localBuildPath}`);
            return localBuildPath;
        }
        // 4. Final fallback - assume it's in the system path
        console.warn(`Native library not found in expected locations, trying: ${filename}`);
        return filename;
    }
    fileExists(filePath) {
        try {
            const fs = require('fs');
            return fs.existsSync(filePath);
        }
        catch {
            return false;
        }
    }
    throwLastError(prefix) {
        if (!this.isAvailable) {
            throw new Error(`${prefix}: Native resolver not available`);
        }
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
exports.NativeResolver = NativeResolver;
