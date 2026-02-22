/**
 * Local HTTP Example
 *
 * Demonstrates using LocalResolutionProvider with HttpDataSource
 * to fetch configuration from a Superposition server.
 *
 * Usage:
 *   npm run build
 *   node dist/examples/local-http-example.js
 */

import { EvaluationContext } from "@openfeature/server-sdk";
import { HttpDataSource } from "../data-sources/http-data-source.js";
import { LocalResolutionProvider } from "../local-resolution-provider.js";

async function main() {
    console.log("=== Superposition Local HTTP Example ===\n");

    // Create HTTP data source
    const httpSource = new HttpDataSource({
        endpoint: process.env.SUPERPOSITION_ENDPOINT || "http://localhost:8080",
        token: process.env.SUPERPOSITION_TOKEN || "token",
        orgId: process.env.SUPERPOSITION_ORG_ID || "localorg",
        workspaceId: process.env.SUPERPOSITION_WORKSPACE_ID || "dev",
    });

    // Create provider with manual refresh strategy
    const provider = new LocalResolutionProvider(
        httpSource,
        undefined, // no fallback
        { type: "manual" }
    );

    try {
        // Initialize the provider
        console.log("Initializing provider...");
        await provider.initialize();
        console.log("Provider initialized successfully!\n");

        // Create evaluation context
        const context: EvaluationContext = {
            targetingKey: "user-1234",
            userId: "1234",
            region: "us-east-1",
        };

        // Resolve all features
        console.log("Resolving all features...");
        const allConfig = await provider.resolveAllFeatures(context);
        console.log("All config:", JSON.stringify(allConfig, null, 2));
        console.log();

        // Get applicable variants for experiments
        console.log("Getting applicable variants...");
        const variants = await provider.getApplicableVariants(context);
        console.log("Variants:", variants);
        console.log();

        // Demonstrate single flag resolution
        console.log("Resolving specific flags:");
        const boolResult = await provider.resolveBooleanEvaluation(
            "feature_enabled",
            false,
            context
        );
        console.log("  feature_enabled:", boolResult.value);

        const stringResult = await provider.resolveStringEvaluation(
            "theme",
            "default",
            context
        );
        console.log("  theme:", stringResult.value);

        const numberResult = await provider.resolveNumberEvaluation(
            "timeout",
            30,
            context
        );
        console.log("  timeout:", numberResult.value);

    } catch (error) {
        console.error("Error:", error);
    } finally {
        // Cleanup
        console.log("\nClosing provider...");
        await provider.onClose();
        console.log("Done!");
    }
}

main();
