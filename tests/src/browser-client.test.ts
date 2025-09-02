/**
 * Test file for the logic module from javascript-browser client
 */
import { describe, test, expect } from "bun:test";
import logic from "../../clients/javascript-browser/src/logic.ts";
import { CacReader } from "../../clients/javascript-browser/src/index.ts";

const mapConfig = describe("Logic Module Tests", () => {
    describe("logic.apply() function - Full matching", () => {
        test("should return true for exact match", () => {
            const condition = { platform: "web", version: "1.0" };
            const context = { platform: "web", version: "1.0", userId: "123" };

            expect(logic.apply(condition, context)).toBe(true);
        });

        test("should return false when context is missing required key", () => {
            const condition = { platform: "web", version: "1.0" };
            const context = { platform: "web" }; // missing version

            expect(logic.apply(condition, context)).toBe(false);
        });

        test("should return false when values don't match", () => {
            const condition = { platform: "web", version: "1.0" };
            const context = { platform: "mobile", version: "1.0" };

            expect(logic.apply(condition, context)).toBe(false);
        });

        test("should handle variantIds special case with array context", () => {
            const condition = { variantIds: "variant-a" };
            const context = { variantIds: ["variant-a", "variant-b"] };

            expect(logic.apply(condition, context)).toBe(true);
        });

        test("should return false when variantIds not found in array", () => {
            const condition = { variantIds: "variant-c" };
            const context = { variantIds: ["variant-a", "variant-b"] };

            expect(logic.apply(condition, context)).toBe(false);
        });

        test("should handle complex object equality", () => {
            const condition = {
                config: {
                    feature: "enabled",
                    settings: { timeout: 5000 },
                },
            };
            const context = {
                config: {
                    feature: "enabled",
                    settings: { timeout: 5000 },
                },
                userId: "456",
            };

            expect(logic.apply(condition, context)).toBe(true);
        });

        test("should handle array equality", () => {
            const condition = { tags: ["premium", "beta"] };
            const context = { tags: ["premium", "beta"], region: "us" };

            expect(logic.apply(condition, context)).toBe(true);
        });

        test("should return false for different array order", () => {
            const condition = { tags: ["premium", "beta"] };
            const context = { tags: ["beta", "premium"], region: "us" };

            expect(logic.apply(condition, context)).toBe(false);
        });
    });

    describe("logic.partialApply() function - Partial matching", () => {
        test("should return true for exact match", () => {
            const condition = { platform: "web", version: "1.0" };
            const context = { platform: "web", version: "1.0", userId: "123" };

            expect(logic.partialApply(condition, context)).toBe(true);
        });

        test("should return true when context is missing some keys (partial match)", () => {
            const condition = { platform: "web", version: "1.0" };
            const context = { platform: "web" }; // missing version

            expect(logic.partialApply(condition, context)).toBe(true);
        });

        test("should return false when present values don't match", () => {
            const condition = { platform: "web", version: "1.0" };
            const context = { platform: "mobile", version: "1.0" };

            expect(logic.partialApply(condition, context)).toBe(false);
        });

        test("should handle variantIds special case with array context", () => {
            const condition = { variantIds: "variant-a" };
            const context = { variantIds: ["variant-a", "variant-b"] };

            expect(logic.partialApply(condition, context)).toBe(true);
        });

        test("should return false when variantIds not found in array", () => {
            const condition = { variantIds: "variant-c" };
            const context = { variantIds: ["variant-a", "variant-b"] };

            expect(logic.partialApply(condition, context)).toBe(false);
        });

        test("should handle complex object equality with partial matching", () => {
            const condition = {
                config: {
                    feature: "enabled",
                    settings: { timeout: 5000 },
                },
            };
            const context = {
                config: {
                    feature: "enabled",
                    settings: { timeout: 5000 },
                },
                userId: "456",
            };

            expect(logic.partialApply(condition, context)).toBe(true);
        });

        test("should handle array equality with partial matching", () => {
            const condition = { tags: ["premium", "beta"] };
            const context = { tags: ["premium", "beta"], region: "us" };

            expect(logic.partialApply(condition, context)).toBe(true);
        });

        test("should return true when condition has more keys than context", () => {
            const condition = {
                platform: "web",
                version: "1.0",
                feature: "enabled",
            };
            const context = { platform: "web", version: "1.0" }; // missing feature

            expect(logic.partialApply(condition, context)).toBe(true);
        });
    });

    describe("Edge cases", () => {
        test("should handle empty condition", () => {
            const condition = {};
            const context = { platform: "web", version: "1.0" };

            expect(logic.apply(condition, context)).toBe(true);
            expect(logic.partialApply(condition, context)).toBe(true);
        });

        test("should handle empty context", () => {
            const condition = { platform: "web" };
            const context = {};

            expect(logic.apply(condition, context)).toBe(false);
            expect(logic.partialApply(condition, context)).toBe(true);
        });

        test("should handle null values", () => {
            const condition = { value: null };
            const context = { value: null };

            expect(logic.apply(condition, context)).toBe(true);
            expect(logic.partialApply(condition, context)).toBe(true);
        });

        test("should handle boolean values", () => {
            const condition = { enabled: true };
            const context = { enabled: true };

            expect(logic.apply(condition, context)).toBe(true);
            expect(logic.partialApply(condition, context)).toBe(true);
        });

        test("should handle number values", () => {
            const condition = { count: 42 };
            const context = { count: 42 };

            expect(logic.apply(condition, context)).toBe(true);
            expect(logic.partialApply(condition, context)).toBe(true);
        });

        test("should differentiate between string and number", () => {
            const condition = { count: "42" };
            const context = { count: 42 };

            expect(logic.apply(condition, context)).toBe(false);
            expect(logic.partialApply(condition, context)).toBe(false);
        });
    });

    describe("deepEqual() function indirectly", () => {
        test("should handle nested objects", () => {
            const condition = {
                nested: {
                    level1: {
                        level2: "value",
                    },
                },
            };
            const context = {
                nested: {
                    level1: {
                        level2: "value",
                    },
                },
            };

            expect(logic.apply(condition, context)).toBe(true);
        });

        test("should handle arrays with different lengths", () => {
            const condition = { list: [1, 2, 3] };
            const context = { list: [1, 2] };

            expect(logic.apply(condition, context)).toBe(false);
        });

        test("should handle mixed types in arrays", () => {
            const condition = { list: [1, "string", true, null] };
            const context = { list: [1, "string", true, null] };

            expect(logic.apply(condition, context)).toBe(true);
        });
    });
});

describe("CacReader Tests", () => {
    describe("evaluateConfig() function", () => {
        test("should return only default config when no contexts match", () => {
            const configData = {
                contexts: [
                    {
                        id: "7f83b1657ff1fc53b92dc18148a1d65dfc2d4b1fa3d677284addd200126d9069",
                        condition: { 
                            user_email: "john.doe@example.com",
                            platform: "mobile",
                            app_version: "2.1.0",
                            user_segment: "premium"
                        },
                        priority: 0,
                        weight: 0,
                        override_with_keys: ["a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3"]
                    }
                ],
                overrides: {
                    a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3: { feature_enabled: true }
                },
                default_configs: {
                    feature_enabled: false,
                    api_timeout: 5000,
                    max_retries: 3
                }
            };

            const reader = new CacReader(configData);
            const result = reader.evaluateConfig({ 
                user_email: "jane.doe@example.com",
                platform: "web",
                app_version: "1.0.0",
                user_segment: "basic"
            });

            expect(result).toEqual({
                feature_enabled: false,
                api_timeout: 5000,
                max_retries: 3
            });
        });

        test("should merge single matching context override with default config", () => {
            const configData = {
                contexts: [
                    {
                        id: "ef92b778bafe771e89245b89ecbc08a44a4e166c06659911881f383d4473e94f",
                        condition: { 
                            platform: "web",
                            country: "US",
                            device_type: "desktop",
                            feature_flags: ["new_ui", "dark_mode"]
                        },
                        priority: 0,
                        weight: 0,
                        override_with_keys: ["b17ef6d19c7a5b1ee83b907c595526dcb1eb06db8227d650d5dda0a9f4ce8cd9"]
                    }
                ],
                overrides: {
                    b17ef6d19c7a5b1ee83b907c595526dcb1eb06db8227d650d5dda0a9f4ce8cd9: { 
                        use_dark_theme: true,
                        api_endpoint: "https://web-api.example.com",
                        cache_duration: 3600
                    }
                },
                default_configs: {
                    use_dark_theme: false,
                    api_endpoint: "https://default-api.example.com",
                    connection_timeout: 10000,
                    cache_duration: 1800
                }
            };

            const reader = new CacReader(configData);
            const result = reader.evaluateConfig({ 
                platform: "web",
                country: "US",
                device_type: "desktop",
                feature_flags: ["new_ui", "dark_mode"]
            });

            expect(result).toEqual({
                use_dark_theme: true,
                api_endpoint: "https://web-api.example.com",
                connection_timeout: 10000,
                cache_duration: 3600
            });
        });

        test("should merge multiple matching context overrides with default config", () => {
            const configData = {
                contexts: [
                    {
                        id: "cd2eb0837c9b4c962c22d2ff8b5441b7b45805887f051d39bf133b583baf6860",
                        condition: { 
                            user_role: "admin",
                            department: "engineering",
                            access_level: 5
                        },
                        priority: 0,
                        weight: 0,
                        override_with_keys: ["9a78211436f65a84b5cdac8cb6b3b6c74ea32c9b4b4b70cf2b6ab82b9ea74e46"]
                    },
                    {
                        id: "f2ca1bb6c7e907d06dafe4687e579fce76b37e4e93b7605022da52e6ccc26fd2",
                        condition: { 
                            environment: "production",
                            region: "us-west-2",
                            cluster: "main",
                            load_balancer: "active"
                        },
                        priority: 1,
                        weight: 1,
                        override_with_keys: ["d4735e3a265e16eee03f59718b9b5d03019c07d8b6c51f90da3a666eec13ab35"]
                    }
                ],
                overrides: {
                    "9a78211436f65a84b5cdac8cb6b3b6c74ea32c9b4b4b70cf2b6ab82b9ea74e46": { 
                        use_admin_features: true,
                        debug_mode: true,
                        max_file_size: 100000000
                    },
                    d4735e3a265e16eee03f59718b9b5d03019c07d8b6c51f90da3a666eec13ab35: {
                        debug_mode: false, // This should override admin setting
                        use_ssl: true,
                        request_timeout: 30000
                    }
                },
                default_configs: {
                    use_admin_features: false,
                    debug_mode: true,
                    use_ssl: false,
                    log_level: "info",
                    max_file_size: 10000000,
                    request_timeout: 5000
                }
            };

            const reader = new CacReader(configData);
            const result = reader.evaluateConfig({ 
                user_role: "admin", 
                department: "engineering",
                access_level: 5,
                environment: "production",
                region: "us-west-2",
                cluster: "main",
                load_balancer: "active"
            });

            expect(result).toEqual({
                use_admin_features: true,
                debug_mode: false, // Last override wins
                use_ssl: true,
                log_level: "info",
                max_file_size: 100000000,
                request_timeout: 30000
            });
        });

        test("should handle contexts with multiple override keys and complex conditions", () => {
            const configData = {
                contexts: [
                    {
                        id: "4e07408562bedb8b60ce05c1decfe3ad16b72230967de01f640b7e4729b49fce",
                        condition: { 
                            user_tier: "premium",
                            subscription_type: "annual",
                            payment_method: "credit_card",
                            account_age_months: 12,
                            total_spent: 1000
                        },
                        priority: 0,
                        weight: 0,
                        override_with_keys: [
                            "ee26b0dd4af7e749aa1a8ee3c10ae9923f618980772e473f8819a5d4940e0db27ac185f8a0e1d5f84f88bc887fd67b143732c304cc5fa9ad8e6f57f50028a8ff",
                            "b14ca5898a4e4133bbce2ea2315a1916d02f4f7defa96b6985e0e69b6fec67b4"
                        ]
                    }
                ],
                overrides: {
                    ee26b0dd4af7e749aa1a8ee3c10ae9923f618980772e473f8819a5d4940e0db27ac185f8a0e1d5f84f88bc887fd67b143732c304cc5fa9ad8e6f57f50028a8ff: { 
                        use_advanced_analytics: true,
                        use_priority_support: true,
                        feature_beta_access: true
                    },
                    b14ca5898a4e4133bbce2ea2315a1916d02f4f7defa96b6985e0e69b6fec67b4: {
                        max_api_calls: 10000,
                        use_advanced_analytics: false, // This should override the previous setting
                        concurrent_sessions: 5,
                        storage_quota_gb: 100
                    }
                },
                default_configs: {
                    use_advanced_analytics: false,
                    use_priority_support: false,
                    max_api_calls: 1000,
                    feature_beta_access: false,
                    concurrent_sessions: 1,
                    storage_quota_gb: 10
                }
            };

            const reader = new CacReader(configData);
            const result = reader.evaluateConfig({ 
                user_tier: "premium",
                subscription_type: "annual",
                payment_method: "credit_card",
                account_age_months: 12,
                total_spent: 1000
            });

            expect(result).toEqual({
                use_advanced_analytics: false, // Last override in array wins
                use_priority_support: true,
                max_api_calls: 10000,
                feature_beta_access: true,
                concurrent_sessions: 5,
                storage_quota_gb: 100
            });
        });

        test("should handle flattened AI configuration with complex conditions", () => {
            const configData = {
                contexts: [
                    {
                        id: "1b4f0e9851971998e732078544c96b36c3d01cedf7caa332359d6f1d83567014",
                        condition: { 
                            feature_flag: "ai_enabled",
                            organization_tier: "enterprise",
                            model_access: "gpt4",
                            region: "us-east-1",
                            compliance_mode: "strict"
                        },
                        priority: 0,
                        weight: 0,
                        override_with_keys: ["ac3478d69a3c81fa62e60f5c3696165a4e5e6ac4e6c59c785ea47f4c26b90ac"]
                    }
                ],
                overrides: {
                    ac3478d69a3c81fa62e60f5c3696165a4e5e6ac4e6c59c785ea47f4c26b90ac: {
                        ai_model_name: "gpt-4-turbo",
                        ai_temperature: 0.7,
                        ai_max_tokens: 2000,
                        ai_top_p: 0.9,
                        rate_limit_per_minute: 100,
                        rate_limit_burst: 150,
                        content_filtering: "strict"
                    }
                },
                default_configs: {
                    ai_model_name: "gpt-3.5-turbo",
                    ai_temperature: 0.3,
                    ai_max_tokens: 1000,
                    ai_top_p: 1.0,
                    ai_timeout: 30,
                    rate_limit_per_minute: 60,
                    rate_limit_concurrent: 5,
                    rate_limit_burst: 80,
                    use_caching: true,
                    content_filtering: "moderate"
                }
            };

            const reader = new CacReader(configData);
            const result = reader.evaluateConfig({ 
                feature_flag: "ai_enabled",
                organization_tier: "enterprise",
                model_access: "gpt4",
                region: "us-east-1",
                compliance_mode: "strict"
            });

            expect(result).toEqual({
                ai_model_name: "gpt-4-turbo",
                ai_temperature: 0.7,
                ai_max_tokens: 2000,
                ai_top_p: 0.9,
                ai_timeout: 30, // Preserved from default
                rate_limit_per_minute: 100,
                rate_limit_concurrent: 5, // Preserved from default
                rate_limit_burst: 150,
                use_caching: true,
                content_filtering: "strict"
            });
        });

        test("should handle complex beta user conditions with arrays", () => {
            const configData = {
                contexts: [
                    {
                        id: "c775e7b757ede630cd0aa1113bd102661ab38829ca52a6422ab782862f268646",
                        condition: { 
                            beta_user: true,
                            user_groups: ["early_adopters", "power_users"],
                            app_version: "3.0.0-beta",
                            device_capabilities: ["webgl", "webrtc", "notifications"],
                            consent_level: "full"
                        },
                        priority: 0,
                        weight: 0,
                        override_with_keys: ["2cf24dba4f21d4288094c45a1d1b5ae1c41d68a6f4b1b1eb6e4b8b1d17b9a4b5"]
                    }
                ],
                overrides: {
                    "2cf24dba4f21d4288094c45a1d1b5ae1c41d68a6f4b1b1eb6e4b8b1d17b9a4b5": {
                        enabled_features: ["new_dashboard", "advanced_charts", "ai_insights", "real_time_collab"],
                        disabled_features: ["legacy_reports", "old_export"],
                        beta_features_access: true,
                        analytics_level: "detailed"
                    }
                },
                default_configs: {
                    enabled_features: ["basic_dashboard", "simple_charts"],
                    disabled_features: [],
                    theme_mode: "light",
                    beta_features_access: false,
                    analytics_level: "basic"
                }
            };

            const reader = new CacReader(configData);
            const result = reader.evaluateConfig({ 
                beta_user: true,
                user_groups: ["early_adopters", "power_users"],
                app_version: "3.0.0-beta",
                device_capabilities: ["webgl", "webrtc", "notifications"],
                consent_level: "full"
            });

            expect(result).toEqual({
                enabled_features: ["new_dashboard", "advanced_charts", "ai_insights", "real_time_collab"],
                disabled_features: ["legacy_reports", "old_export"],
                theme_mode: "light",
                beta_features_access: true,
                analytics_level: "detailed"
            });
        });

        test("should handle variantIds condition matching with complex experiment setup", () => {
            const configData = {
                contexts: [
                    {
                        id: "8b1a9953c4611296a827abf8c47804d7e824e7e5dd8b02b67dc8e2ec0f75d6c3",
                        condition: { 
                            variantIds: "variant_a",
                            experiment_group: "checkout_flow",
                            user_segment: "high_value",
                            traffic_allocation: 50
                        },
                        priority: 0,
                        weight: 0,
                        override_with_keys: ["fb6d74b05b78f62cd63ed1c0c7892b4b9b6c9d9f4b8b2f9e5b1d47a3a5c2b8e4"]
                    }
                ],
                overrides: {
                    fb6d74b05b78f62cd63ed1c0c7892b4b9b6c9d9f4b8b2f9e5b1d47a3a5c2b8e4: {
                        use_new_algorithm: true,
                        button_color: "#007bff",
                        experiment_tracking_id: "variant_a",
                        checkout_steps: 3,
                        payment_methods: ["card", "paypal", "apple_pay"]
                    }
                },
                default_configs: {
                    use_new_algorithm: false,
                    button_color: "#6c757d",
                    experiment_tracking_id: "control",
                    checkout_steps: 5,
                    payment_methods: ["card", "paypal"]
                }
            };

            const reader = new CacReader(configData);
            const result = reader.evaluateConfig({ 
                variantIds: ["variant_a", "variant_b"],
                experiment_group: "checkout_flow",
                user_segment: "high_value",
                traffic_allocation: 50
            });

            expect(result).toEqual({
                use_new_algorithm: true,
                button_color: "#007bff",
                experiment_tracking_id: "variant_a",
                checkout_steps: 3,
                payment_methods: ["card", "paypal", "apple_pay"]
            });
        });

        test("should handle variantIds special case - single variant in array", () => {
            const configData = {
                contexts: [
                    {
                        id: "9c4a8d2f1e3b7a6f5d8e2c1a9b7f4e6d3c8a5b2f9e1d7c4a6b8f3e5d2a9c7b4",
                        condition: { 
                            variantIds: "test_variant_x"
                        },
                        priority: 0,
                        weight: 0,
                        override_with_keys: ["a1b2c3d4e5f6789012345678901234567890abcdef1234567890abcdef123456"]
                    }
                ],
                overrides: {
                    a1b2c3d4e5f6789012345678901234567890abcdef1234567890abcdef123456: {
                        test_feature_enabled: true,
                        tracking_pixel: "variant_x_pixel.js"
                    }
                },
                default_configs: {
                    test_feature_enabled: false,
                    tracking_pixel: "control_pixel.js"
                }
            };

            const reader = new CacReader(configData);
            const result = reader.evaluateConfig({ 
                variantIds: ["test_variant_x"]
            });

            expect(result).toEqual({
                test_feature_enabled: true,
                tracking_pixel: "variant_x_pixel.js"
            });
        });

        test("should handle variantIds special case - variant not in array", () => {
            const configData = {
                contexts: [
                    {
                        id: "a5f3e7b9c2d8f1a4e6b3c9d7f2a8e5b1c4d6f9a2e7b4c8d1f5a3e9b6c2d7f8a1",
                        condition: { 
                            variantIds: "missing_variant"
                        },
                        priority: 0,
                        weight: 0,
                        override_with_keys: ["f1e2d3c4b5a6987654321098765432109876543210fedcba9876543210fedcba"]
                    }
                ],
                overrides: {
                    f1e2d3c4b5a6987654321098765432109876543210fedcba9876543210fedcba: {
                        should_not_apply: true
                    }
                },
                default_configs: {
                    should_not_apply: false,
                    default_value: "control"
                }
            };

            const reader = new CacReader(configData);
            const result = reader.evaluateConfig({ 
                variantIds: ["variant_y", "variant_z"]
            });

            expect(result).toEqual({
                should_not_apply: false,
                default_value: "control"
            });
        });

        test("should handle variantIds with complex multi-variant experiment", () => {
            const configData = {
                contexts: [
                    {
                        id: "b6d4f8a2e9c1f7b3d5a8e4c2f9b6d1a7e3c8f5b2d9a4e7c1f8b5d2a6e9c3f7b4",
                        condition: { 
                            variantIds: "homepage_v2",
                            experiment_name: "homepage_redesign",
                            traffic_split: "b_variant"
                        },
                        priority: 0,
                        weight: 0,
                        override_with_keys: ["c7a5e9f3b1d8c4f6a2e7b9d5c3f8a6e1b4d7c9f2a5e8b3d6c1f4a9e2b5d8c7f1"]
                    }
                ],
                overrides: {
                    c7a5e9f3b1d8c4f6a2e7b9d5c3f8a6e1b4d7c9f2a5e8b3d6c1f4a9e2b5d8c7f1: {
                        homepage_layout: "grid",
                        hero_banner_size: "large",
                        cta_button_text: "Get Started Now",
                        show_testimonials: true,
                        newsletter_popup_delay: 10000
                    }
                },
                default_configs: {
                    homepage_layout: "list",
                    hero_banner_size: "medium",
                    cta_button_text: "Sign Up",
                    show_testimonials: false,
                    newsletter_popup_delay: 30000
                }
            };

            const reader = new CacReader(configData);
            const result = reader.evaluateConfig({ 
                variantIds: ["homepage_v1", "homepage_v2", "checkout_v3"],
                experiment_name: "homepage_redesign",
                traffic_split: "b_variant"
            });

            expect(result).toEqual({
                homepage_layout: "grid",
                hero_banner_size: "large",
                cta_button_text: "Get Started Now",
                show_testimonials: true,
                newsletter_popup_delay: 10000
            });
        });

        test("should handle empty contexts array", () => {
            const configData = {
                contexts: [],
                overrides: {},
                default_configs: {
                    app_name: "MyApp",
                    app_version: "1.0.0",
                    use_analytics: true
                }
            };

            const reader = new CacReader(configData);
            const result = reader.evaluateConfig({ user_type: "guest" });

            expect(result).toEqual({
                app_name: "MyApp",
                app_version: "1.0.0",
                use_analytics: true
            });
        });

        test("should handle context condition evaluation errors gracefully", () => {
            const configData = {
                contexts: [
                    {
                        id: "a3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855",
                        condition: { 
                            region: "us-east-1",
                            availability_zone: "us-east-1a",
                            instance_type: "t3.large"
                        },
                        priority: 0,
                        weight: 0,
                        override_with_keys: ["e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"]
                    }
                ],
                overrides: {
                    e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855: { 
                        cdn_endpoint: "https://us-east-1.cdn.example.com",
                        edge_location: "virginia"
                    }
                },
                default_configs: {
                    cdn_endpoint: "https://global.cdn.example.com",
                    edge_location: "global"
                }
            };

            const reader = new CacReader(configData);
            
            // This should work normally even if there are internal errors
            const result = reader.evaluateConfig({ 
                region: "us-east-1",
                availability_zone: "us-east-1a",
                instance_type: "t3.large"
            });
            
            expect(result).toEqual({
                cdn_endpoint: "https://us-east-1.cdn.example.com",
                edge_location: "virginia"
            });
        });

        test("should preserve order of context evaluation with complex conditions", () => {
            const configData = {
                contexts: [
                    {
                        id: "3e23e8160039594a33894f6564e1b1348bbd7a0088d42c4acb73eeaed59c009d",
                        condition: { 
                            environment: "staging",
                            deployment_version: "v2.1.0",
                            feature_branch: "main"
                        },
                        priority: 0,
                        weight: 0,
                        override_with_keys: ["2e99758548972a8e8822ad47fa1017ff72f06f3ff6a016851f45c398732bc50c"]
                    },
                    {
                        id: "f9b14e7d85c4f2b2f7b2d9d1d5e1f3a2b8c7d4e5f6a7b8c9d0e1f2a3b4c5d6e7",
                        condition: { 
                            environment: "staging", // Same condition, later in array
                            deployment_version: "v2.1.0",
                            feature_branch: "main"
                        },
                        priority: 1,
                        weight: 1,
                        override_with_keys: ["1a79a4d60de6718e8e5b326e338ae533b62d0d13fb05d25b3f7c0b1a2e3d4f5g"]
                    }
                ],
                overrides: {
                    "2e99758548972a8e8822ad47fa1017ff72f06f3ff6a016851f45c398732bc50c": { 
                        log_level: "debug",
                        database_url: "staging-db-primary.example.com",
                        redis_cluster: "staging-redis-001"
                    },
                    "1a79a4d60de6718e8e5b326e338ae533b62d0d13fb05d25b3f7c0b1a2e3d4f5g": { 
                        log_level: "info", // This should override the previous setting
                        cache_ttl: 300,
                        monitoring_enabled: true
                    }
                },
                default_configs: {
                    log_level: "error",
                    database_url: "prod-db.example.com",
                    cache_ttl: 3600,
                    redis_cluster: "prod-redis-001",
                    monitoring_enabled: false
                }
            };

            const reader = new CacReader(configData);
            const result = reader.evaluateConfig({ 
                environment: "staging",
                deployment_version: "v2.1.0",
                feature_branch: "main"
            });

            // The second override should win due to order
            expect(result).toEqual({
                log_level: "info", // Last override wins
                database_url: "staging-db-primary.example.com",
                cache_ttl: 300,
                redis_cluster: "staging-redis-001",
                monitoring_enabled: true
            });
        });
    });
});
