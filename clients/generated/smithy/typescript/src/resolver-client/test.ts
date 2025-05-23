import { superpositionClient } from './index';

async function runTests() {
    console.log("Testing new SuperpositionClient interface...");

    try {
        // Create configuration client
        const configurationClient = superpositionClient.getConfigurationClient({
            defaults: {
                defaultConfigs: { "feature.enabled": true },
                contexts: [],
                overrides: {}
            },
            cache: { ttl: 60000 },
            evaluationCache: { ttl: 30000, size: 100 }
        });

        // Test context
        const context = {
            "os": "android",
            "clientId": "test"
        };

        // Test basic eval
        console.log("Testing basic eval...");
        const config = await configurationClient.eval(context);
        console.log("Basic config:", JSON.stringify(config, null, 2));

        // Test typed access
        console.log("Testing typed access...");
        const featureEnabled = await configurationClient.getBooleanValue(
            "feature.enabled",
            false,
            context
        );
        console.log("Feature enabled:", featureEnabled);

        // Test filtered config
        console.log("Testing filtered config...");
        const filteredConfig = await configurationClient.getFiltered(context, ["feature"]);
        console.log("Filtered config:", JSON.stringify(filteredConfig, null, 2));

        // Test with refresh
        console.log("Testing with refresh...");
        const refreshedConfig = await configurationClient.eval(context, true);
        console.log("Refreshed config:", JSON.stringify(refreshedConfig, null, 2));

        console.log("All tests passed!");
    } catch (error) {
        console.error("Test failed:", error);
    }
}

runTests().catch(console.error);