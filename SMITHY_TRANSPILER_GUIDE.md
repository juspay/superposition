# Smithy Transpiler for Superposition

## Overview

This guide explains the new Smithy Transpiler system that allows you to write API interfaces in Rust and automatically transpile them to Smithy IDL specifications.

## Architecture

```
┌─────────────────────┐
│   Rust API Code     │
│  (with macros)      │
└──────────┬──────────┘
           │
           ▼
┌─────────────────────┐
│  Proc Macros        │
│  - smithy_service   │
│  - smithy_resource  │
│  - smithy_operation │
│  - SmithyShape      │
│  - SmithyEnum       │
└──────────┬──────────┘
           │
           ▼
┌─────────────────────┐
│  Metadata Registry  │
│  - Collects all     │
│    definitions      │
└──────────┬──────────┘
           │
           ▼
┌─────────────────────┐
│  Smithy IDL Gen     │
│  - Generates .smithy│
│    files            │
└──────────┬──────────┘
           │
           ▼
┌─────────────────────┐
│  .smithy files      │
│  (same format as    │
│   existing ones)    │
└─────────────────────┘
```

## Motivation

The Superposition project currently uses Smithy as the source of truth for API definitions, with code generation going from Smithy → Rust (and other languages). However, there are scenarios where you might want to go the other direction:

1. **Rust-First Development**: Define APIs in Rust with full type checking and IDE support
2. **Type Safety**: Catch API definition errors at compile time
3. **Reduced Duplication**: Single source of truth in code you're already writing
4. **Refactoring Support**: Use Rust's refactoring tools on your API definitions
5. **Documentation**: Generate Smithy for documentation and tooling

## Key Components

### 1. Core Crate: `smithy_transpiler`

Location: `/crates/smithy_transpiler/`

This crate provides:
- Procedural macros for annotating Rust code
- Metadata extraction from Rust types
- Smithy IDL code generation
- Type mapping between Rust and Smithy

### 2. Metadata Structures

The transpiler uses metadata structures to represent Smithy concepts:

- `ServiceMeta`: Service definitions
- `ResourceMeta`: Resource definitions
- `OperationMeta`: Operation definitions
- `StructMeta`: Structure/shape definitions
- `EnumMeta`: Enum definitions
- `ErrorMeta`: Error definitions

### 3. Procedural Macros

#### `#[smithy_service]`

Marks a struct as a Smithy service:

```rust
#[smithy_service(
    namespace = "io.superposition.new_api",
    version = "2024-01-01",
    protocol = "restJson1"
)]
pub struct NewApiService;
```

#### `#[smithy_resource]`

Defines a Smithy resource with CRUD operations:

```rust
#[smithy_resource(
    identifiers = "id: String, workspace_id: String",
    read = "GetUser",
    list = "ListUsers",
    create = "CreateUser",
    update = "UpdateUser",
    delete = "DeleteUser"
)]
pub struct User {
    pub id: String,
    pub workspace_id: String,
    pub name: String,
    pub email: String,
}
```

#### `#[smithy_operation]`

Defines an API operation:

```rust
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

    #[smithy(http_header = "x-workspace")]
    pub workspace_id: String,
}
```

#### `#[derive(SmithyShape)]`

Generates Smithy shape definition:

```rust
#[derive(SmithyShape)]
#[smithy(namespace = "io.superposition")]
pub struct UserData {
    #[smithy(required)]
    pub name: String,

    pub email: Option<String>,

    pub roles: Vec<String>,
}
```

#### `#[derive(SmithyEnum)]`

Generates Smithy enum:

```rust
#[derive(SmithyEnum)]
#[smithy(namespace = "io.superposition")]
pub enum UserRole {
    #[smithy(value = "admin")]
    Admin,

    #[smithy(value = "user")]
    User,
}
```

## Usage Example

### Step 1: Define Your API in Rust

```rust
// api_definitions.rs
use smithy_transpiler::{smithy_service, smithy_operation, SmithyShape, SmithyEnum};

#[smithy_service(
    namespace = "io.superposition.features",
    version = "2024-01-01",
    protocol = "restJson1"
)]
pub struct FeaturesService;

#[derive(SmithyEnum)]
#[smithy(namespace = "io.superposition.features")]
pub enum FeatureStatus {
    #[smithy(value = "enabled")]
    Enabled,

    #[smithy(value = "disabled")]
    Disabled,
}

#[derive(SmithyShape)]
#[smithy(namespace = "io.superposition.features")]
pub struct Feature {
    #[smithy(required)]
    pub id: String,

    #[smithy(required)]
    pub name: String,

    pub status: FeatureStatus,
}

#[smithy_operation(
    http_method = "POST",
    uri = "/features",
    tags = "Feature Management"
)]
pub struct CreateFeature {
    #[smithy(http_header = "x-workspace")]
    pub workspace_id: String,

    #[smithy(http_payload)]
    pub feature: Feature,
}
```

### Step 2: Generate Smithy IDL

Option A: Build Script (`build.rs`):

```rust
use smithy_transpiler::{SmithyRegistry, generate_smithy_idl};

fn main() {
    let mut registry = SmithyRegistry::new();

    // Register all your types
    FeaturesService::register_smithy_service(&mut registry);
    Feature::register_smithy_shape(&mut registry);
    CreateFeature::register_smithy_operation(&mut registry);

    // Generate Smithy IDL
    let smithy_code = generate_smithy_idl(&registry);

    // Write to file
    std::fs::write("smithy/models/features.smithy", smithy_code)
        .expect("Failed to write Smithy file");
}
```

Option B: Standalone Binary:

```rust
fn main() {
    let mut registry = SmithyRegistry::new();
    // ... register types ...
    let smithy_code = generate_smithy_idl(&registry);
    println!("{}", smithy_code);
}
```

### Step 3: Output

The generated `features.smithy` file:

```smithy
$version: "2.0"

namespace io.superposition.features

@restJson1
service FeaturesService {
    version: "2024-01-01"
    operations: [
        CreateFeature
    ]
}

enum FeatureStatus {
    Enabled = "enabled"
    Disabled = "disabled"
}

structure Feature {
    @required
    id: String

    @required
    name: String

    status: FeatureStatus
}

@http(method: "POST", uri: "/features")
@tags(["Feature Management"])
operation CreateFeature {
    input := {
        @httpHeader("x-workspace")
        workspace_id: String

        @httpPayload
        feature: Feature
    }
}
```

## Type Mapping

The transpiler automatically maps Rust types to Smithy types:

| Rust                 | Smithy          |
|----------------------|-----------------|
| `String`             | `String`        |
| `i32`, `i64`         | `Integer`       |
| `f32`, `f64`         | `Float`         |
| `bool`               | `Boolean`       |
| `Vec<T>`             | `TList`         |
| `HashMap<K, V>`      | `Map<K, V>`     |
| `Option<T>`          | `T` (optional)  |
| Custom structs       | `structure`     |
| Custom enums         | `enum`          |

## Integration with Existing Smithy Workflow

This transpiler complements (doesn't replace) the existing Smithy workflow:

### Traditional Workflow (Preserved)
```
.smithy files → smithy-build → Rust SDK + Other SDKs
```

### New Transpiler Workflow (Additional)
```
Rust API defs → smithy_transpiler → .smithy files
```

### Combined Workflow
```
Rust API defs → smithy_transpiler → .smithy files → smithy-build → SDKs
```

You can:
1. Use transpiler for new APIs while keeping existing .smithy files
2. Mix hand-written .smithy with transpiled .smithy
3. Use transpiled .smithy as input to existing smithy-build pipeline

## Comparison with Current Approach

### Current: Smithy → Rust

**Pros:**
- Language-agnostic API definitions
- Well-established tooling
- Multi-language SDK generation

**Cons:**
- Need to learn Smithy IDL syntax
- Separate files to maintain
- No compile-time checking of Smithy files
- Limited IDE support for .smithy files

### New: Rust → Smithy

**Pros:**
- Define APIs in Rust with full type checking
- IDE autocomplete, refactoring, etc.
- Catch errors at compile time
- Single source of truth in code
- Can still generate .smithy for tooling

**Cons:**
- Rust-specific (but you can still use generated .smithy elsewhere)
- Less mature than Smithy tooling
- May not support all advanced Smithy features initially

## Best Use Cases

Use the transpiler when:

1. **Prototyping**: Quickly define APIs in Rust during development
2. **Rust-Only Services**: Internal services that don't need multi-language SDKs
3. **Type Safety**: You want compile-time guarantees about API definitions
4. **Documentation**: Generate Smithy specs from existing Rust code

Keep using traditional Smithy when:

1. **Multi-Language Focus**: Primary concern is SDK generation for many languages
2. **Complex Smithy Features**: Need advanced Smithy features not yet supported
3. **API-First Design**: Want to design APIs before implementation
4. **Existing Codebase**: Already have extensive .smithy files

## Examples

See these examples for complete working code:

1. **Simple Example**: `/crates/smithy_transpiler/examples/simple.rs`
   - Direct API usage
   - Programmatic Smithy generation
   - Weather API example

2. **Full Example**: `/examples/smithy_transpiler_example/`
   - Blog API with CRUD operations
   - Build script integration
   - Complete service definition

Run examples:

```bash
# Simple programmatic example
cargo run --example simple --manifest-path crates/smithy_transpiler/Cargo.toml

# Full example with build integration
cd examples/smithy_transpiler_example
cargo build
cargo run
```

## Limitations

Current limitations (may be addressed in future versions):

1. Only supports Smithy 2.0 IDL format
2. Best suited for REST JSON APIs (restJson1 protocol)
3. Some advanced Smithy features may require manual .smithy editing
4. Mixins and inheritance have limited support
5. Custom traits may need manual definition

## Future Enhancements

Potential improvements:

- [ ] Support for more protocols (gRPC, GraphQL)
- [ ] Advanced mixin and inheritance support
- [ ] Custom trait definitions
- [ ] Validation rules and constraints
- [ ] Streaming operations
- [ ] Event streams
- [ ] More comprehensive error definitions
- [ ] Integration with smithy-build.json
- [ ] CLI tool for batch transpilation

## Contributing

The transpiler is designed to be extensible. Key extension points:

1. **Parser** (`parser.rs`): Add new attribute parsing
2. **Metadata** (`metadata.rs`): Add new metadata types
3. **Codegen** (`codegen.rs`): Add new Smithy IDL generation patterns

## References

- [Smithy Specification](https://smithy.io/2.0/spec/)
- [Smithy IDL](https://smithy.io/2.0/spec/idl.html)
- [Superposition Smithy Models](./smithy/models/)
- [Transpiler README](./crates/smithy_transpiler/README.md)
