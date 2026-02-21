/**
 * Polling Example
 *
 * Demonstrates the polling refresh strategy with LocalResolutionProvider
 * using the OpenFeature client interface.
 *
 * This example connects to a Superposition server via HTTP, polls for config
 * changes every 10 seconds, and prints a config value in a loop using the
 * standard OpenFeature client API. Change the config on the server and watch
 * the printed value update automatically.
 *
 * Usage:
 *   npm run build
 *   node dist/examples/polling-example.js
 *
 * Environment variables (all optional, with defaults shown):
 *   SUPERPOSITION_ENDPOINT  http://localhost:8080
 *   SUPERPOSITION_TOKEN     token
 *   SUPERPOSITION_ORG_ID    localorg
 *   SUPERPOSITION_WORKSPACE dev
 *   POLL_INTERVAL           10        (seconds between server polls)
 *   PRINT_INTERVAL          5         (seconds between printing the value)
 *   CONFIG_KEY              max_connections   (the config key to watch)
 */

import { OpenFeature } from "@openfeature/server-sdk";
import { HttpDataSource } from "../data-sources";
import { LocalResolutionProvider } from "../local-resolution-provider";

function envOr(key: string, defaultValue: string): string {
    return process.env[key] || defaultValue;
}

async function sleep(ms: number): Promise<void> {
    return new Promise((resolve) => setTimeout(resolve, ms));
}

async function main() {
    const endpoint = envOr("SUPERPOSITION_ENDPOINT", "http://localhost:8080");
    const token = envOr("SUPERPOSITION_TOKEN", "token");
    const orgId = envOr("SUPERPOSITION_ORG_ID", "localorg");
    const workspaceId = envOr("SUPERPOSITION_WORKSPACE", "dev");
    const pollInterval = parseInt(envOr("POLL_INTERVAL", "10"), 10);
    const printInterval = parseInt(envOr("PRINT_INTERVAL", "5"), 10);
    const configKey = envOr("CONFIG_KEY", "max_connections");

    console.log("=== Superposition Polling Example ===");
    console.log(`Endpoint:        ${endpoint}`);
    console.log(`Org / Workspace: ${orgId} / ${workspaceId}`);
    console.log(`Poll interval:   ${pollInterval}s`);
    console.log(`Print interval:  ${printInterval}s`);
    console.log(`Watching key:    ${configKey}`);
    console.log();

    // Create HTTP data source
    const httpSource = new HttpDataSource({
        endpoint,
        token,
        orgId,
        workspaceId,
    });

    // Create provider with polling strategy
    const provider = new LocalResolutionProvider(
        httpSource,
        undefined, // no fallback
        { type: "polling", interval: pollInterval }
    );

    try {
        // Register with OpenFeature and create a client
        await OpenFeature.setProviderAndWait(provider);
        const client = OpenFeature.getClient();

        // Allow time for the provider to initialize via OpenFeature
        await sleep(2000);

        console.log(
            `Provider ready. Printing config every ${printInterval}s (Ctrl-C to stop).\n`
        );

        const context = {
            targetingKey: "user-123",
        };

        // Print config value in a loop
        let iteration = 0;
        while (iteration < 20) {
            // Run for ~2 minutes max (20 * 5s)
            const timestamp = new Date().toISOString().split("T")[1].split(".")[0];

            try {
                const value = await client.getNumberValue(
                    configKey,
                    100, // default value
                    context
                );
                console.log(`[${timestamp}] ${configKey} = ${value}`);
            } catch (e) {
                console.error(
                    `[${timestamp}] Error resolving ${configKey}:`,
                    e
                );
            }

            await sleep(printInterval * 1000);
            iteration++;
        }

        console.log("\nExample completed!");
    } catch (error) {
        console.error("Error:", error);
    } finally {
        // Cleanup
        await provider.onClose();
    }
}

main();
