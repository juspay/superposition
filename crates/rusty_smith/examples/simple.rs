// Simple example showing Rusty Smith API usage
//
// This demonstrates how to programmatically build a Smithy specification
// from Rust data structures.

use rusty_smith::{
    codegen::generate_smithy_idl, EnumMeta, EnumVariantMeta, FieldMeta, HttpBinding, HttpMethod,
    OperationMeta, ServiceMeta, SmithyRegistry, StructMeta,
};
use syn::{parse_quote, Ident, Type};

fn main() {
    let mut registry = SmithyRegistry::new();

    // Register a service
    registry.register_service(ServiceMeta {
        name: Ident::new("WeatherService", proc_macro2::Span::call_site()),
        namespace: "com.example.weather".to_string(),
        version: "2024-01-01".to_string(),
        protocol: "restJson1".to_string(),
        resources: vec![],
        operations: vec!["GetWeather".to_string()],
        auth: vec![],
        title: Some("Weather API".to_string()),
        documentation: Some("Provides weather forecasts and current conditions".to_string()),
    });

    // Register an enum
    registry.register_enum(EnumMeta {
        name: Ident::new("WeatherCondition", proc_macro2::Span::call_site()),
        variants: vec![
            EnumVariantMeta {
                name: "Sunny".to_string(),
                value: Some("sunny".to_string()),
                documentation: Some("Clear skies".to_string()),
            },
            EnumVariantMeta {
                name: "Cloudy".to_string(),
                value: Some("cloudy".to_string()),
                documentation: Some("Overcast conditions".to_string()),
            },
            EnumVariantMeta {
                name: "Rainy".to_string(),
                value: Some("rainy".to_string()),
                documentation: Some("Precipitation".to_string()),
            },
        ],
        namespace: Some("com.example.weather".to_string()),
        documentation: Some("Weather condition types".to_string()),
    });

    // Register a shape
    let string_type: Type = parse_quote!(String);
    let i32_type: Type = parse_quote!(i32);
    let f32_type: Type = parse_quote!(f32);

    registry.register_shape(StructMeta {
        name: Ident::new("WeatherData", proc_macro2::Span::call_site()),
        fields: vec![
            FieldMeta {
                name: "location".to_string(),
                ty: string_type.clone(),
                required: true,
                http_binding: None,
                documentation: Some("Location name".to_string()),
                default_value: None,
                not_property: false,
            },
            FieldMeta {
                name: "temperature".to_string(),
                ty: f32_type,
                required: true,
                http_binding: None,
                documentation: Some("Temperature in Celsius".to_string()),
                default_value: None,
                not_property: false,
            },
            FieldMeta {
                name: "humidity".to_string(),
                ty: i32_type,
                required: true,
                http_binding: None,
                documentation: Some("Humidity percentage".to_string()),
                default_value: None,
                not_property: false,
            },
        ],
        namespace: Some("com.example.weather".to_string()),
        mixins: vec![],
        documentation: Some("Current weather data".to_string()),
    });

    // Register an operation
    registry.register_operation(OperationMeta {
        name: Ident::new("GetWeather", proc_macro2::Span::call_site()),
        http_method: HttpMethod::GET,
        uri: "/weather/{city}".to_string(),
        input: Some(StructMeta {
            name: Ident::new("GetWeatherInput", proc_macro2::Span::call_site()),
            fields: vec![
                FieldMeta {
                    name: "city".to_string(),
                    ty: string_type.clone(),
                    required: true,
                    http_binding: Some(HttpBinding::Label),
                    documentation: Some("City name".to_string()),
                    default_value: None,
                    not_property: false,
                },
                FieldMeta {
                    name: "units".to_string(),
                    ty: string_type.clone(),
                    required: false,
                    http_binding: Some(HttpBinding::Query("units".to_string())),
                    documentation: Some("Temperature units (celsius/fahrenheit)".to_string()),
                    default_value: None,
                    not_property: false,
                },
            ],
            namespace: None,
            mixins: vec![],
            documentation: None,
        }),
        output: Some(StructMeta {
            name: Ident::new("GetWeatherOutput", proc_macro2::Span::call_site()),
            fields: vec![FieldMeta {
                name: "weather".to_string(),
                ty: parse_quote!(WeatherData),
                required: true,
                http_binding: Some(HttpBinding::Payload),
                documentation: Some("Weather data".to_string()),
                default_value: None,
                not_property: false,
            }],
            namespace: None,
            mixins: vec![],
            documentation: None,
        }),
        errors: vec![],
        tags: vec!["Weather".to_string()],
        readonly: true,
        idempotent: false,
        documentation: Some("Get current weather for a city".to_string()),
    });

    // Generate Smithy IDL
    let smithy_code = generate_smithy_idl(&registry);

    println!("Generated Smithy IDL:");
    println!("====================\n");
    println!("{}", smithy_code);

    // Optionally write to file
    std::fs::write("weather.smithy", &smithy_code).expect("Failed to write Smithy file");
    println!("\n✅ Written to weather.smithy");
}
