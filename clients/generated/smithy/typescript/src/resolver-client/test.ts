import { ConfigurationClient } from './configuration-client';
import { NativeResolver } from './native-resolver';

async function testConfig() {
    console.log("🧪 Testing improved ConfigurationClient...");

    // Initialize with your server config
    const config = {
        endpoint: 'http://localhost:8080',
        workspace_id: 'dev',  // Maps to x-tenant
        org_id: 'localorg',
        token: { token: '12345678' }
    };

    const resolver = new NativeResolver();
    const client = new ConfigurationClient(config, resolver);

    try {
        // Test context that should match your server conditions
        const context = {
            "clientId": "meesho"
        };

        console.log("🎯 Evaluating context:", context);

        // This should now show the full data flow
        const result = await client.eval(context);

        console.log("✅ Final result:", JSON.stringify(result, null, 2));

        // Test specific key access
        const key2Value = await client.getStringValue("key2", "default", context);
        console.log("🔑 key2 value:", key2Value);

        if (key2Value === "value2") {
            console.log("🎉 SUCCESS! Got expected value: value5");
        } else {
            console.log("❌ FAILED! Expected 'value5', got:", key2Value);
        }

    } catch (error) {
        console.error("❌ Test failed:", error);
        console.error(error.stack);
    }
}

testConfig().catch(console.error);