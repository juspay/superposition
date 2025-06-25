import { SuperpositionClient, GetResolvedConfigCommand, GetResolvedConfigCommandInput } from '@io.juspay/superposition-sdk';

async function testConfig() {
    console.log("🧪 Testing ConfigurationClient with Smithy client...");

    // Server config details
    const endpoint = 'http://localhost:8080';
    const workspace_id = 'dev';
    const org_id = 'localorg';
    const tokenIdentity = { token: '12345678' };

    // Initialize Smithy Client
    const smithyClient = new SuperpositionClient({
        endpoint: endpoint,
        token: tokenIdentity,
    });

    try {
        // Test context that should match your server conditions
        const context = {
            "clientId": "meesho"
        };

        console.log("🎯 Evaluating context:", context);

        // Prepare input for GetResolvedConfigCommand
        const commandInput: GetResolvedConfigCommandInput = {
            workspace_id: workspace_id,
            org_id: org_id,
            context: context,
        };

        const command = new GetResolvedConfigCommand(commandInput);
        // This should now show the full data flow using Smithy client
        const response = await smithyClient.send(command);

        const resolvedConfig = response.config as Record<string, any> | undefined;

        console.log("✅ Full resolved config:", JSON.stringify(resolvedConfig, null, 2));

        // Test specific key access (emulating getStringValue)
        const keyToTest = "key2";
        const defaultValue = "default";
        let key2Value = defaultValue;

        if (resolvedConfig && typeof resolvedConfig === 'object' && resolvedConfig !== null && Object.prototype.hasOwnProperty.call(resolvedConfig, keyToTest)) {
            const val = resolvedConfig[keyToTest];
            if (typeof val === 'string') {
                key2Value = val;
            } else if (val !== undefined && val !== null) {
                key2Value = String(val); // Coerce to string to mimic getStringValue
            }
            // If val is null, key2Value will be "null". If undefined or key not present, it remains defaultValue.
        }
        console.log(`🔑 ${keyToTest} value:`, key2Value);

        // Original test logic condition, with consistent messaging
        if (key2Value === "value2") {
            console.log(`🎉 SUCCESS! For ${keyToTest}, got expected value: 'value2'`);
        } else {
            // The original FAILED message mentioned "Expected 'value5'". 
            // If 'value5' is the actual target, the condition key2Value === "value5" should be used.
            // This message is now consistent with the check for 'value2'.
            console.log(`❌ FAILED! For ${keyToTest}, expected 'value2', got:`, key2Value);
        }

    } catch (error) {
        console.error("❌ Test failed:", error);
        // Ensure error.stack is checked for existence before logging
        if (error instanceof Error && error.stack) {
            console.error(error.stack);
        }
    }
}

testConfig().catch(console.error);