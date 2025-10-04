---
sidebar_position: 1
title: Superposition with TOML
---

# Toml formatted CAC configuration file in a simple rust application
This document shows how to use a TOML formatted Superposition configuration file in your Rust application.

## Add dependencies to your Cargo.toml

```toml
[dependencies]
superposition_toml = { git = "https://github.com/juspay/superposition.git", branch = "main" }
serde_json = { version = "1.0.140" }
```

## Include a simple context-aware-configuration file

Create a configuration file called `example.cac.toml` with the following content.

```toml
[default-config]
per_km_rate = { "value" = 20.0, "schema" = { "type" = "number" } }
surge_factor = { "value" = 0.0, "schema" = { "type" = "number" } }

[dimensions]
city = { schema = { "type" = "string", "enum" = ["Bangalore", "Delhi"] } }
vehicle_type = { schema = { "type" = "string", "enum" = [ "auto", "cab", "bike", ] } }
hour_of_day = { schema = { "type" = "integer", "minimum" = 0, "maximum" = 23 }}

[context."vehicle_type=cab"]
per_km_rate = 25.0

[context."vehicle_type=bike"]
per_km_rate = 15.0

[context."city=Bangalore; vehicle_type=cab"]
per_km_rate = 22.0

[context."city=Delhi; vehicle_type=cab; hour_of_day=18"]
surge_factor = 5.0

[context."city=Delhi; vehicle_type=cab; hour_of_day=6"]
surge_factor = 5.0
```

## Use it in your application with a sample context

Use the above configuration file in a simple rust application (`src/main.rs`) to resolve the configuration for a given context i.e. one or dimensions.  In this case - get configuration when `vehicle_type` is `cab`.

```rust
use serde_json::{Map, Value};
use superposition_toml::SuperpositionToml;
use std::collections::HashMap;
use std::process;

fn main() {
    let file: String = String::from("example.cac.toml");

    let mut dimensions: Map<String, Value> = Map::new();
    dimensions.insert(String::from("vehicle_type"), Value::String(String::from("cab")));

    let config = SuperpositionToml::parse(&file).unwrap_or_else(|_err| {
        eprintln!("Could not parse file at {}", file);
        process::exit(-1);
    });

    println!("{:#?}", config.get_resolved_config(&dimensions));
    process::exit(0);
}
```

## Build and run the application

```bash
cargo run 

```

You should see the following output:

```
{
    "per_km_rate": Float(
        25.0,
    ),
    "surge_factor": Float(
        0.0,
    ),
}
```
