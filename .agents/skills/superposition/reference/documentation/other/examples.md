---
sidebar_position: 10
title: Examples
description: Complete working examples of SuperTOML configurations
---

# Examples

This page contains complete, working examples of SuperTOML configurations for different use cases.

## Example 1: Ride-Hailing Pricing

A configuration for a ride-hailing application with city, vehicle type, and time-based pricing.

### Configuration File

```toml
# ride-pricing.toml

[default-configs]
per_km_rate = { value = 20.0, schema = { type = "number", minimum = 0, description = "Rate per kilometer in local currency" } }
surge_factor = { value = 0.0, schema = { type = "number", minimum = 0, maximum = 10, description = "Surge multiplier (0 = no surge)" } }
base_fare = { value = 50.0, schema = { type = "number", minimum = 0, description = "Base fare for all rides" } }
waiting_charge = { value = 2.0, schema = { type = "number", minimum = 0, description = "Charge per minute of waiting" } }

[dimensions]
city = {
    position = 4,
    schema = { type = "string", enum = ["Bangalore", "Delhi", "Chennai", "Mumbai"] }
}
vehicle_type = {
    position = 2,
    schema = { type = "string", enum = ["auto", "cab", "bike", "premium"] }
}
hour_of_day = {
    position = 3,
    schema = { type = "integer", minimum = 0, maximum = 23 }
}

# Derived dimension for time periods
time_period = {
    position = 1,
    type = "LOCAL_COHORT:hour_of_day",
    schema = {
        type = "string",
        enum = ["morning_rush", "evening_rush", "off_peak"],
        definitions = {
            morning_rush = { and = [
                { ">=" = [{ var = "hour_of_day" }, 7] },
                { "<=" = [{ var = "hour_of_day" }, 10] }
            ]},
            evening_rush = { and = [
                { ">=" = [{ var = "hour_of_day" }, 17] },
                { "<=" = [{ var = "hour_of_day" }, 21] }
            ]}
        }
    }
}

# Derived dimension for city regions
region = {
    position = 1,
    type = "LOCAL_COHORT:city",
    schema = {
        type = "string",
        enum = ["south", "north", "west", "otherwise"],
        definitions = {
            south = { in = [{ var = "city" }, ["Bangalore", "Chennai"]] },
            north = { in = [{ var = "city" }, ["Delhi"]] },
            west = { in = [{ var = "city" }, ["Mumbai"]] }
        }
    }
}

# Vehicle type overrides
[[overrides]]
_context_ = { vehicle_type = "bike" }
per_km_rate = 12.0
base_fare = 30.0

[[overrides]]
_context_ = { vehicle_type = "auto" }
per_km_rate = 15.0
base_fare = 35.0

[[overrides]]
_context_ = { vehicle_type = "cab" }
per_km_rate = 20.0
base_fare = 50.0

[[overrides]]
_context_ = { vehicle_type = "premium" }
per_km_rate = 35.0
base_fare = 100.0
waiting_charge = 5.0

# City-specific overrides
[[overrides]]
_context_ = { city = "Bangalore" }
per_km_rate = 18.0

[[overrides]]
_context_ = { city = "Delhi" }
base_fare = 60.0

[[overrides]]
_context_ = { city = "Mumbai" }
per_km_rate = 22.0
base_fare = 55.0

# City + vehicle combinations
[[overrides]]
_context_ = { city = "Bangalore", vehicle_type = "cab" }
per_km_rate = 16.0

[[overrides]]
_context_ = { city = "Delhi", vehicle_type = "cab" }
per_km_rate = 22.0

# Time-based surge
[[overrides]]
_context_ = { time_period = "morning_rush" }
surge_factor = 1.5

[[overrides]]
_context_ = { time_period = "evening_rush" }
surge_factor = 2.0

# Specific time overrides
[[overrides]]
_context_ = { city = "Delhi", vehicle_type = "cab", hour_of_day = 18 }
surge_factor = 3.0

[[overrides]]
_context_ = { city = "Delhi", vehicle_type = "cab", hour_of_day = 19 }
surge_factor = 3.0

# Regional pricing
[[overrides]]
_context_ = { region = "south" }
waiting_charge = 1.5
```

### Resolution Examples

| Context                                                        | per_km_rate | surge_factor | base_fare |
| -------------------------------------------------------------- | ----------- | ------------ | --------- |
| `{ city: "Bangalore", vehicle_type: "cab", hour_of_day: 14 }`  | 16.0        | 0.0          | 50.0      |
| `{ city: "Delhi", vehicle_type: "cab", hour_of_day: 18 }`      | 22.0        | 3.0          | 60.0      |
| `{ city: "Chennai", vehicle_type: "bike", hour_of_day: 8 }`    | 12.0        | 1.5          | 30.0      |
| `{ city: "Mumbai", vehicle_type: "premium", hour_of_day: 20 }` | 35.0        | 2.0          | 100.0     |

---

## Example 2: Multi-Tenant SaaS

A configuration for a multi-tenant SaaS application with feature flags and limits.

### Configuration File

```toml
# saas-config.toml

[default-configs]
max_users = { value = 10, schema = { type = "integer", minimum = 1, description = "Maximum number of users" } }
max_storage_gb = { value = 5, schema = { type = "integer", minimum = 1, description = "Maximum storage in GB" } }
api_rate_limit = { value = 100, schema = { type = "integer", minimum = 10, description = "API requests per minute" } }
feature_analytics = { value = false, schema = { type = "boolean", description = "Enable analytics dashboard" } }
feature_sso = { value = false, schema = { type = "boolean", description = "Enable SSO integration" } }
feature_api_access = { value = true, schema = { type = "boolean", description = "Enable API access" } }
support_level = { value = "email", schema = { type = "string", enum = ["email", "chat", "phone", "priority"], description = "Support level" } }
backup_retention_days = { value = 7, schema = { type = "integer", minimum = 1, maximum = 365, description = "Backup retention in days" } }

[dimensions]
tenant_tier = {
    position = 5,
    schema = { type = "string", enum = ["free", "starter", "professional", "enterprise"] }
}
region = {
    position = 3,
    schema = { type = "string", enum = ["us", "eu", "apac"] }
}
tenant_id = {
    position = 10,
    schema = { type = "string", pattern = "^tenant_[a-z0-9]+$" }
}

# Tier-based limits
[[overrides]]
_context_ = { tenant_tier = "free" }
max_users = 1
max_storage_gb = 1
api_rate_limit = 50
feature_analytics = false
feature_sso = false
support_level = "email"
backup_retention_days = 3

[[overrides]]
_context_ = { tenant_tier = "starter" }
max_users = 10
max_storage_gb = 10
api_rate_limit = 100
feature_analytics = true
feature_sso = false
support_level = "email"
backup_retention_days = 7

[[overrides]]
_context_ = { tenant_tier = "professional" }
max_users = 50
max_storage_gb = 100
api_rate_limit = 500
feature_analytics = true
feature_sso = true
support_level = "chat"
backup_retention_days = 30

[[overrides]]
_context_ = { tenant_tier = "enterprise" }
max_users = 1000
max_storage_gb = 1000
api_rate_limit = 5000
feature_analytics = true
feature_sso = true
feature_api_access = true
support_level = "priority"
backup_retention_days = 90

# Region-specific settings
[[overrides]]
_context_ = { region = "eu" }
backup_retention_days = 30  # GDPR compliance

[[overrides]]
_context_ = { region = "apac" }
support_level = "email"  # Different time zones

# Enterprise + EU combination
[[overrides]]
_context_ = { tenant_tier = "enterprise", region = "eu" }
backup_retention_days = 365  # Extended retention for EU enterprise

# Specific tenant overrides (highest priority)
[[overrides]]
_context_ = { tenant_id = "tenant_acme_corp" }
max_users = 5000
support_level = "phone"
```

---

## Example 3: Feature Flags

A configuration for feature flag management with gradual rollouts.

### Configuration File

```toml
# feature-flags.toml

[default-configs]
new_dashboard_enabled = { value = false, schema = { type = "boolean" } }
checkout_v2_enabled = { value = false, schema = { type = "boolean" } }
recommendation_engine = { value = "off", schema = { type = "string", enum = ["off", "basic", "advanced"] } }
dark_mode = { value = false, schema = { type = "boolean" } }
beta_features = { value = [], schema = { type = "array", items = { type = "string" } } }

[dimensions]
environment = {
    position = 1,
    schema = { type = "string", enum = ["development", "staging", "production"] }
}
user_segment = {
    position = 3,
    schema = { type = "string", enum = ["internal", "beta", "premium", "regular"] }
}
user_id = {
    position = 5,
    schema = { type = "string" }
}
country = {
    position = 2,
    schema = { type = "string" }
}

# Environment-based flags
[[overrides]]
_context_ = { environment = "development" }
new_dashboard_enabled = true
checkout_v2_enabled = true
recommendation_engine = "advanced"
dark_mode = true
beta_features = ["all"]

[[overrides]]
_context_ = { environment = "staging" }
new_dashboard_enabled = true
checkout_v2_enabled = true
recommendation_engine = "basic"

# User segment-based flags
[[overrides]]
_context_ = { user_segment = "internal" }
new_dashboard_enabled = true
checkout_v2_enabled = true
recommendation_engine = "advanced"
dark_mode = true

[[overrides]]
_context_ = { user_segment = "beta" }
new_dashboard_enabled = true
checkout_v2_enabled = true
recommendation_engine = "basic"
beta_features = ["new_search", "improved_checkout"]

[[overrides]]
_context_ = { user_segment = "premium" }
recommendation_engine = "advanced"
dark_mode = true

# Country-specific rollouts
[[overrides]]
_context_ = { country = "US", user_segment = "regular" }
new_dashboard_enabled = true

[[overrides]]
_context_ = { country = "IN" }
recommendation_engine = "basic"

# Production rollout (10% - specific user IDs)
[[overrides]]
_context_ = { environment = "production", user_id = "user_001" }
checkout_v2_enabled = true

[[overrides]]
_context_ = { environment = "production", user_id = "user_042" }
checkout_v2_enabled = true

[[overrides]]
_context_ = { environment = "production", user_id = "user_133" }
checkout_v2_enabled = true
```

---

## Example 4: Environment Configuration

A configuration for application settings across environments.

### Configuration File

```toml
# app-config.toml

[default-configs]
log_level = { value = "info", schema = { type = "string", enum = ["debug", "info", "warn", "error"] } }
database_pool_size = { value = 10, schema = { type = "integer", minimum = 1, maximum = 100 } }
cache_ttl_seconds = { value = 300, schema = { type = "integer", minimum = 0 } }
api_timeout_ms = { value = 5000, schema = { type = "integer", minimum = 100 } }
enable_profiling = { value = false, schema = { type = "boolean" } }
cors_origins = { value = [], schema = { type = "array", items = { type = "string" } } }
rate_limit_requests = { value = 100, schema = { type = "integer", minimum = 0 } }
rate_limit_window_ms = { value = 60000, schema = { type = "integer", minimum = 1000 } }

[dimensions]
environment = {
    position = 1,
    schema = { type = "string", enum = ["local", "development", "staging", "production"] }
}
service = {
    position = 2,
    schema = { type = "string", enum = ["api", "worker", "scheduler", "admin"] }
}
region = {
    position = 3,
    schema = { type = "string", enum = ["us-east-1", "us-west-2", "eu-west-1", "ap-south-1"] }
}

# Environment overrides
[[overrides]]
_context_ = { environment = "local" }
log_level = "debug"
database_pool_size = 2
cache_ttl_seconds = 0
enable_profiling = true
cors_origins = ["http://localhost:3000", "http://localhost:8080"]

[[overrides]]
_context_ = { environment = "development" }
log_level = "debug"
database_pool_size = 5
cache_ttl_seconds = 60
enable_profiling = true
cors_origins = ["https://dev.example.com"]

[[overrides]]
_context_ = { environment = "staging" }
log_level = "info"
database_pool_size = 10
cors_origins = ["https://staging.example.com"]

[[overrides]]
_context_ = { environment = "production" }
log_level = "warn"
database_pool_size = 50
cache_ttl_seconds = 600
rate_limit_requests = 1000
cors_origins = ["https://example.com", "https://app.example.com"]

# Service-specific overrides
[[overrides]]
_context_ = { service = "worker" }
log_level = "info"
database_pool_size = 5

[[overrides]]
_context_ = { service = "scheduler" }
log_level = "warn"
database_pool_size = 2

[[overrides]]
_context_ = { service = "admin" }
enable_profiling = false
rate_limit_requests = 50

# Region-specific settings
[[overrides]]
_context_ = { region = "eu-west-1" }
cache_ttl_seconds = 900  # Longer cache for EU latency

# Combinations
[[overrides]]
_context_ = { environment = "production", service = "api" }
database_pool_size = 100
rate_limit_requests = 5000

[[overrides]]
_context_ = { environment = "production", region = "ap-south-1" }
api_timeout_ms = 10000  # Longer timeout for Asia
```

---

## Running the Examples

### Rust

Save the configuration to a file (e.g., `config.toml`) and run:

```rust
use std::fs;
use superposition_core::{ConfigFormat, TomlFormat, eval_config, MergeStrategy};
use serde_json::{Map, Value};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let content = fs::read_to_string("config.toml")?;
    let config = TomlFormat::parse_config(&content)?;

    // Build your runtime context
    let mut context = Map::new();
    context.insert("city".to_string(), Value::String("Bangalore".to_string()));
    context.insert("vehicle_type".to_string(), Value::String("cab".to_string()));
    context.insert("hour_of_day".to_string(), Value::Number(18.into()));

    // Resolve the configuration
    let result = eval_config(
        (*config.default_configs).clone(),
        &config.contexts,
        &config.overrides,
        &config.dimensions,
        &context,
        MergeStrategy::MERGE,
        None,
    )?;

    println!("Resolved configuration:");
    for (key, value) in &result {
        println!("  {}: {}", key, value);
    }

    Ok(())
}
```

### Using the Example Binary

The `superposition_config_file_examples` crate provides a ready-to-run example:

```bash
cd examples/superposition_config_file_examples
cargo run
```

This will parse both `example.toml` and `example.json` and demonstrate various resolution scenarios.
