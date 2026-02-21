/**
 * Local with Fallback Example
 *
 * Demonstrates using LocalResolutionProvider with HTTP primary source
 * and File fallback source. If the HTTP server is unavailable, the
 * provider falls back to reading from a local config file.
 *
 * Usage:
 *   npm run build
 *   node dist/examples/local-with-fallback-example.js
 */

import { OpenFeature } from "@openfeature/server-sdk";
import { HttpDataSource, FileDataSource } from "../data-sources";
import { LocalResolutionProvider } from "../local-resolution-provider";
import * as path from "path";

async function sleep(ms: number): Promise<void> {
    return new Promise((resolve) => setTimeout(resolve, ms));
}

async function main() {
    console.log("=== Superposition Fallback + Polling Example ===\n");

    // Create HTTP data source (primary)
    const httpSource = new HttpDataSource({
        endpoint: process.env.SUPERPOSITION_ENDPOINT || "http://localhost:8080",
        token: process.env.SUPERPOSITION_TOKEN || "token",
        orgId: process.env.SUPERPOSITION_ORG_ID || "localorg",
        workspaceId: process.env.SUPERPOSITION_WORKSPACE_ID || "dev",
    });

    // Create File data source (fallback)
    const configPath = path.join(__dirname, "config.toml");
    console.log(`Primary: HTTP (${httpSource.endpoint})`);
    console.log(`Fallback: ${configPath}`);
    console.log(`Polling every 10s. Printing currency and timeout values (Ctrl-C to stop).\n`);

    const fileSource = new FileDataSource(configPath);

    // Create provider with both sources and polling strategy
    const provider = new LocalResolutionProvider(
        httpSource,
        fileSource, // fallback
        { type: "polling", interval: 10 }
    );

    try {
        // Register with OpenFeature and create a client
        await OpenFeature.setProviderAndWait(provider);
        const client = OpenFeature.getClient();

        // Allow time for the provider to initialize via OpenFeature
        await sleep(2000);

        const context = {
            targetingKey: "user-456",
            os: "linux",
            city: "Berlin",
        };

        console.log("Starting polling loop...\n");

        // Poll for config changes and print values in a loop
        let iteration = 0;
        while (iteration < 12) {
            // Run for 1 minute (12 * 5s)
            const timestamp = new Date().toISOString().split("T")[1].split(".")[0];

            try {
                const currency = await client.getStringValue(
                    "currency",
                    "USD",
                    context
                );
                process.stdout.write(`[${timestamp}] currency = ${currency}`);
            } catch (e) {
                process.stdout.write(`[${timestamp}] currency error: ${e}`);
            }

            try {
                const timeout = await client.getNumberValue("timeout", 30, context);
                console.log(`  |  timeout = ${timeout}`);
            } catch (e) {
                console.log(`  |  timeout error: ${e}`);
            }

            await sleep(5000);
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
