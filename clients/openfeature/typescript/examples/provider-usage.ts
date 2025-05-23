import { OpenFeature } from '@openfeature/server-sdk';
import { SuperpositionProviderOptions, SuperpositionProvider } from '../src/resolver-client/superposition-provider';



async function testSuperpositionProvider() {
    console.log("Testing SuperpositionProvider...");

    const config: SuperpositionProviderOptions = {
        endpoint: 'http://localhost:8080',
        workspace_id: 'dev',
        org_id: 'localorg',
        token: '12345678',
        fallbackConfig: {
            "contexts": [
                {
                    "id": "fddcf0e1dea48e2a66a7536d52a6897623b75263d853b4f0216e2f733d29eb5e",
                    "condition": {
                        "and": [
                            {
                                "==": [
                                    {
                                        "var": "clientId"
                                    },
                                    "Flipkart"
                                ]
                            }
                        ]
                    },
                    "priority": 0,
                    "weight": 0,
                    "override_with_keys": [
                        "7b930a9e6c337bcec143243c4f6d179ff229cc617dc48e6a51e75cdf94f77430"
                    ]
                },
                {
                    "id": "6f856bd1775dd6033d63ee66282743dfe79a38aa661c2f1a7b58bfdb2079bf18",
                    "condition": {
                        "and": [
                            {
                                "==": [
                                    {
                                        "var": "clientId"
                                    },
                                    "flipkart"
                                ]
                            }
                        ]
                    },
                    "priority": 1,
                    "weight": 1,
                    "override_with_keys": [
                        "0db5ba3cc1050e9b59888c7f336a4acd179c175e91a7065503dd3fcdc1aae7c9"
                    ]
                },
                {
                    "id": "06e586719fe33d4ed221130c858cceeeeb724f76fa6ffae84bd990e274199317",
                    "condition": {
                        "and": [
                            {
                                "==": [
                                    {
                                        "var": "clientId"
                                    },
                                    "amazon"
                                ]
                            }
                        ]
                    },
                    "priority": 2,
                    "weight": 2,
                    "override_with_keys": [
                        "5a4ade3036bed1e33b10569b4fc7d72cd0651d33dd6b2ba6ec73f2385b961bb4"
                    ]
                },
                {
                    "id": "31b2d57af6e58dc9bc943916346cace7a8ed622665e8654d77f39c04886a57c9",
                    "condition": {
                        "and": [
                            {
                                "==": [
                                    {
                                        "var": "clientId"
                                    },
                                    "meesho"
                                ]
                            },
                            {
                                "==": [
                                    {
                                        "var": "os"
                                    },
                                    "android"
                                ]
                            }
                        ]
                    },
                    "priority": 3,
                    "weight": 3,
                    "override_with_keys": [
                        "6c118e1acb09952266f1fed0486bb6e3f59ed231ae9fd21dbefa12b230b29654"
                    ]
                }
            ],
            "overrides": {
                "7b930a9e6c337bcec143243c4f6d179ff229cc617dc48e6a51e75cdf94f77430": {
                    "feature.enabled": true,
                    "key2": "value6"
                },
                "5a4ade3036bed1e33b10569b4fc7d72cd0651d33dd6b2ba6ec73f2385b961bb4": {
                    "key2": "value76"
                },
                "0db5ba3cc1050e9b59888c7f336a4acd179c175e91a7065503dd3fcdc1aae7c9": {
                    "key2": "value10"
                },
                "6c118e1acb09952266f1fed0486bb6e3f59ed231ae9fd21dbefa12b230b29654": {
                    "key2": "value6"
                }
            },
            "default_configs": {
                "feature.enabled": true,
                "key2": "value2"
            }
        },
        refreshStrategy: {
            interval: 30000,
            timeout: 5000,
        },
        evaluationCache: {
            ttl: 30000,
            size: 100,
        },
        experimentationOptions: {
            refreshStrategy: {
                interval: 10000,
                timeout: 5000,
            },
            evaluationCache: {
                ttl: 30000,
                size: 100,
            }
        }
    };

    const provider = new SuperpositionProvider(config);

    try {
        console.log("🔄 Setting and initializing provider...");
        await OpenFeature.setProviderAndWait(provider);
        console.log("✅ Provider initialized and ready");

        const client = OpenFeature.getClient();

        const context = {
            "clientId": "meesho",
            "os": "android",
        };

        console.log("Evaluating context:", context);

        const Key2Value = await client.getStringValue("key2", "default_value", context);
        console.log("Key2 Value:", Key2Value);

        if (Key2Value === "value7") {
            console.log("✅ Test passed: Key2 has expected value 'value7'");
        } else {
            console.log("❌ Test failed: Key2 does not have expected value 'value7'");
        }


        const booleanFlag = await client.getBooleanValue("feature.enabled", false, context);
        console.log("boolean flag:", booleanFlag);

        const stringFlag = await client.getStringValue("defaultTheme", "dark", context);
        console.log("string flag:", stringFlag);

        const numberFlag = await client.getNumberValue("max_retries", 3, context);
        console.log("number flag:", numberFlag);

        const objectFlag = await client.getObjectValue("app_config", {}, context);
        // console.log("object flag:", JSON.stringify(objectFlag, null, 2));

        console.log("🎉 All evaluations completed successfully!");
        setInterval(async () => {
            const Key2Value = await client.getStringValue("key2", "default_value", context);
            console.log("Key2 Value:", Key2Value);
        }, 10000);

    } catch (error) {
        console.error("Test Failed:", error);
        if (error instanceof Error && error.stack) {
            console.error(error.stack);
        }
    }
}


testSuperpositionProvider().catch(console.error); 
