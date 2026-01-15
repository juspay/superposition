# Rusty Smith 🦀

A Rust-based DSL for writing API interfaces that transpile to [Smithy IDL](https://smithy.io) specifications.

## Overview

This crate provides procedural macros and code generation tools that allow you to define APIs using Rust structs, enums, and attributes, then automatically transpile them into Smithy IDL format. This approach offers several benefits:

- **Type safety**: Define your APIs in Rust with full type checking
- **Single source of truth**: Your Rust code is the source, Smithy files are generated
- **IDE support**: Full Rust tooling support (autocomplete, refactoring, etc.)
- **Reduced duplication**: No need to maintain separate IDL files
- **Familiar syntax**: Use Rust patterns you already know

## Features

- ✅ Service definitions with protocols and authentication
- ✅ Resource definitions with CRUD operations
- ✅ Operation definitions with HTTP bindings
- ✅ Structure (shape) definitions with documentation
- ✅ Enum definitions with custom values
- ✅ HTTP bindings (headers, query params, path labels, payloads)
- ✅ Automatic type mapping (String, Integer, Boolean, Lists, Maps)
- ✅ Documentation extraction from Rust doc comments
- ✅ Mixins and inheritance patterns
- ✅ Error definitions with HTTP status codes

## Installation

Add to your `Cargo.toml`:

```toml
[dependencies]
rusty-smith = { path = "path/to/rusty_smith" }
```

## Quick Start

### 1. Define a Service

```rust
use rusty_smith::smithy_service;

#[smithy_service(
    namespace = "com.example.myapi",
    version = "2024-01-01",
    protocol = "restJson1"
)]
pub struct MyApiService;
```

### 2. Define Data Structures

```rust
use rusty_smith::SmithyShape;

/// A user in the system
#[derive(SmithyShape)]
#[smithy(namespace = "com.example.myapi")]
pub struct User {
    /// Unique user identifier
    #[smithy(required)]
    pub id: String,

    /// User's display name
    #[smithy(required)]
    pub name: String,

    /// User's email address
    pub email: Option<String>,

    /// User roles
    pub roles: Vec<String>,
}
```

### 3. Define Enums

```rust
use rusty_smith::SmithyEnum;

#[derive(SmithyEnum)]
#[smithy(namespace = "com.example.myapi")]
pub enum UserStatus {
    #[smithy(value = "active")]
    Active,

    #[smithy(value = "inactive")]
    Inactive,
}
```

### 4. Define Operations

```rust
use rusty_smith::smithy_operation;

/// Creates a new user
#[smithy_operation(
    http_method = "POST",
    uri = "/users",
    tags = "User Management"
)]
pub struct CreateUser {
    #[smithy(http_header = "x-api-key")]
    pub api_key: String,

    #[smithy(http_payload)]
    pub user: User,
}

/// Gets a user by ID
#[smithy_operation(
    http_method = "GET",
    uri = "/users/{id}",
    tags = "User Management",
    readonly
)]
pub struct GetUser {
    #[smithy(http_label)]
    pub id: String,

    #[smithy(http_query = "include_roles")]
    pub include_roles: Option<bool>,
}
```

### 5. Define Resources

```rust
use rusty_smith::smithy_resource;

#[smithy_resource(
    identifiers = "id: String",
    read = "GetUser",
    list = "ListUsers",
    create = "CreateUser",
    update = "UpdateUser",
    delete = "DeleteUser"
)]
pub struct UserResource {
    pub id: String,
    pub name: String,
    pub email: String,
}
```

## Generated Smithy Output

The above Rust code transpiles to:

```smithy
$version: "2.0"

namespace com.example.myapi

@restJson1
service MyApiService {
    version: "2024-01-01"
    resources: [
        UserResource
    ]
}

@documentation("A user in the system")
structure User {
    @required
    @documentation("Unique user identifier")
    id: String

    @required
    @documentation("User's display name")
    name: String

    @documentation("User's email address")
    email: String

    @documentation("User roles")
    roles: StringList
}

enum UserStatus {
    Active = "active"
    Inactive = "inactive"
}

@documentation("Creates a new user")
@http(method: "POST", uri: "/users")
@tags(["User Management"])
operation CreateUser {
    input := {
        @httpHeader("x-api-key")
        api_key: String

        @httpPayload
        user: User
    }
}

@documentation("Gets a user by ID")
@http(method: "GET", uri: "/users/{id}")
@tags(["User Management"])
@readonly
operation GetUser {
    input := {
        @httpLabel
        id: String

        @httpQuery("include_roles")
        include_roles: Boolean
    }
}

resource UserResource {
    identifiers: {
        id: String
    }
    read: GetUser
    list: ListUsers
    create: CreateUser
    update: UpdateUser
    delete: DeleteUser
}
```

## Attribute Reference

### Service Attributes

```rust
#[smithy_service(
    namespace = "com.example.api",  // Required: Smithy namespace
    version = "2024-01-01",          // Required: API version
    protocol = "restJson1",          // Optional: Protocol (default: restJson1)
    title = "My API"                 // Optional: Service title
)]
```

### Resource Attributes

```rust
#[smithy_resource(
    identifiers = "id: String, workspace: String",  // Resource identifiers
    read = "GetOperation",                           // Read operation
    list = "ListOperation",                          // List operation
    create = "CreateOperation",                      // Create operation
    update = "UpdateOperation",                      // Update operation
    delete = "DeleteOperation"                       // Delete operation
)]
```

### Operation Attributes

```rust
#[smithy_operation(
    http_method = "POST",           // Required: GET, POST, PUT, PATCH, DELETE
    uri = "/path/{id}",             // Required: URI pattern
    tags = "Category, SubCategory", // Optional: Comma-separated tags
    readonly,                       // Optional: Mark as readonly
    idempotent                      // Optional: Mark as idempotent
)]
```

### Field Attributes

```rust
pub struct Example {
    #[smithy(required)]                    // Mark field as required
    field1: String,

    #[smithy(http_header = "x-custom")]    // Bind to HTTP header
    field2: String,

    #[smithy(http_query = "param")]        // Bind to query parameter
    field3: String,

    #[smithy(http_label)]                  // Bind to path parameter
    field4: String,

    #[smithy(http_payload)]                // Mark as HTTP payload
    field5: Data,

    #[smithy(documentation = "Field docs")] // Add documentation
    field6: String,
}
```

## Type Mapping

Rust types are automatically mapped to Smithy types:

| Rust Type | Smithy Type |
|-----------|-------------|
| `String` | `String` |
| `i32`, `i64` | `Integer` |
| `f32`, `f64` | `Float` |
| `bool` | `Boolean` |
| `Vec<T>` | `TList` |
| `HashMap<K, V>` | `Map<K, V>` |
| `Option<T>` | `T` (optional) |
| Custom structs | Structure shapes |
| Custom enums | Enum shapes |

## Build Integration

Create a `build.rs` file to automatically generate Smithy files:

```rust
use rusty_smith::{SmithyRegistry, generate_smithy_idl};
use std::fs;

fn main() {
    let mut registry = SmithyRegistry::new();

    // Register all your types
    MyService::register_smithy_service(&mut registry);
    User::register_smithy_shape(&mut registry);
    CreateUser::register_smithy_operation(&mut registry);

    // Generate Smithy IDL
    let smithy_code = generate_smithy_idl(&registry);

    // Write to file
    fs::write("generated/api.smithy", smithy_code)
        .expect("Failed to write Smithy file");

    println!("cargo:rerun-if-changed=src/api.rs");
}
```

## Comparison with Traditional Smithy

### Traditional Approach (Smithy → Rust)

1. Write `.smithy` files manually
2. Run Smithy codegen to generate Rust code
3. Implement handlers for generated types
4. Keep Smithy and Rust code in sync manually

### Transpiler Approach (Rust → Smithy)

1. Write Rust code with macros
2. Transpiler generates `.smithy` files
3. Use Smithy tooling on generated files (optional)
4. Single source of truth in Rust

## Use Cases

- **Rust-first teams**: Define APIs in Rust, generate Smithy for tooling
- **Documentation**: Generate Smithy specs for documentation tools
- **Multi-language support**: Generate Smithy, then use Smithy codegen for other languages
- **Type safety**: Catch API definition errors at compile time
- **Refactoring**: Use Rust refactoring tools on your API definitions

## Limitations

- Currently supports Smithy 2.0 IDL format
- Some advanced Smithy features may require manual `.smithy` editing
- Best suited for REST JSON APIs (other protocols coming soon)

## Examples

See the `examples/` directory for complete examples:

- `rusty_smith_example`: Blog API example with CRUD operations

## Contributing

Contributions welcome! Areas for improvement:

- Additional protocol support (gRPC, GraphQL)
- More Smithy 2.0 features
- Better error messages
- More comprehensive type mapping

## License

Same license as the Superposition project.

## References

- [Smithy Specification](https://smithy.io/2.0/spec/)
- [Smithy IDL](https://smithy.io/2.0/spec/idl.html)
- [AWS Smithy](https://github.com/smithy-lang/smithy)
