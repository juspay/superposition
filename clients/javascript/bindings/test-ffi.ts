// Create a separate test file to test the FFI directly
import { NativeResolver } from "./native-resolver.js";

async function testFFIDirectly() {
    console.log("Testing FFI directly with known data...");

    const resolver = new NativeResolver();

    // Use the exact data structure from your server response
    const defaultConfigs = {
        key2: "value2",
    };

    const contexts = [
        {
            id: "31b2d57af6e58dc9bc943916346cace7a8ed622665e8654d77f39c04886a57c9",
            condition: {
                clientId: "meesho",
                os: "android"
            },
            priority: 0,
            weight: 0,
            override_with_keys: [
                "832d63717c4358a992bec2d8076cf9ee126f0ef0a53fcb10955ddfea460cdaae",
            ],
        },
    ];

    const overrides = {
        "832d63717c4358a992bec2d8076cf9ee126f0ef0a53fcb10955ddfea460cdaae": {
            key2: "value5",
        },
    };

    const queryData = {
        os: "android",
        clientId: "meesho",
    };

    try {
        console.log("🧪 Testing with exact server data structure...");
        const result = resolver.resolveConfig(
            defaultConfigs,
            contexts,
            overrides,
            {}, // Empty dimensions for this test
            queryData,
            "merge"
        );

        console.log(
            "✅ Direct FFI test result:",
            JSON.stringify(result, null, 2)
        );

        // Should output: {"key2": "value5"}
        if (result.key2 === "value5") {
            console.log("🎉 FFI is working correctly!");
        } else {
            console.log("❌ FFI result doesn't match expected value");
        }
    } catch (error) {
        console.error("❌ Direct FFI test failed:", error);
    }
}

testFFIDirectly().catch(console.error);
