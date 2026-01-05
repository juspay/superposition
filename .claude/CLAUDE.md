# Superposition

## Project Overview

Superposition is a configuration and experimentation management platform that allows software teams to manage their configurations safely and run multi-variate experiments. It emphasizes safety through:

- Strong typing of configuration values via JSON Schema
- Custom validation functions written in JavaScript
- Support for staggering configuration changes via experiments
- Context-aware configurations that cascade from least to most specific (similar to CSS)

## Tech Stack

### Backend
- **Language**: Rust
- **Version**: Defined in `rust-toolchain.toml`
- **Framework**: Actix-web (4.5.0)
- **Database**: PostgreSQL (via Diesel ORM 2.2.4)
- **Cache**: Redis (via fred 9.2.1)
- **Authentication**: Keycloak integration

### Frontend
- **Framework**: Leptos 0.6.11 (Rust-based reactive web framework)
- **Styling**: Tailwind CSS, DaisyUI
- **Build Location**: `crates/frontend`

### Client Libraries
Multiple language support for SDK and OpenFeature providers:
- Rust
- JavaScript/TypeScript (Node.js and browser)
- Python
- Java
- Haskell (WIP)
- Go (TBD)

## Project Structure

```
superposition/
├── crates/                              # Rust workspace crates
│   ├── context_aware_config/           # Core CAC service
│   ├── experimentation_platform/       # Experimentation service
│   ├── frontend/                       # Leptos frontend application
│   ├── service_utils/                  # Shared service utilities
│   ├── superposition/                  # Main superposition crate
│   ├── superposition_types/            # Type definitions
│   ├── superposition_macros/           # Procedural macros
│   ├── superposition_derives/          # Derive macros
│   ├── superposition_core/             # Core logic
│   ├── superposition_provider/         # OpenFeature provider
│   ├── superposition_sdk/              # Client SDK
│   ├── cac_client/                     # CAC client library
│   ├── experimentation_client/         # Experimentation client
│   └── cac_toml/                       # TOML configuration support
├── clients/                            # Multi-language client libraries
│   ├── javascript/                     # JS/TS clients (SDK, provider)
│   ├── javascript-browser/             # Browser-specific JS client
│   ├── python/                         # Python clients
│   ├── java/                           # Java clients
│   └── haskell/                        # Haskell clients (WIP)
├── examples/                           # Example applications
│   ├── dynamic-payment-fields/         # Frontend example
│   ├── k8s-staggered-releaser/        # Infrastructure example
│   └── cac_redis_module/              # Storage integration example
├── smithy/                             # AWS Smithy IDL definitions
├── uniffi/                             # UniFFI bindings for multi-language support
├── postman/                            # API testing collections
├── tests/                              # Integration tests
├── docs/                               # Documentation (website)
├── docker-compose/                     # Docker compose configurations
├── helm/                               # Kubernetes Helm charts
└── scripts/                            # Build and utility scripts
```

## Development Setup

### Prerequisites
- Rust toolchain (see `rust-toolchain.toml`)
- Node.js and npm (for JavaScript clients and frontend tooling)
- Docker and Docker Compose (for local development)
- PostgreSQL
- Redis

### Quick Start with Docker
```bash
docker run -p 8080:8080 ghcr.io/juspay/superposition-demo:latest
```
Access the admin UI at `localhost:8080`

### Local Development Setup
See the full development setup guide at: https://juspay.io/superposition/docs/setup

1. Set up environment variables (copy `.env.example` to `.env`)
2. Start dependencies with Docker Compose:
   ```bash
   docker-compose up -d postgres redis
   ```
3. Run database migrations
4. Build and run the application:
   ```bash
   cargo build
   cargo run --bin juspay_superposition
   ```

## Building

### Rust Backend
```bash
# Build all crates
cargo build

# Build release version
cargo build --release

# Build specific crate
cargo build -p context_aware_config
```

### Frontend (Leptos)
The frontend is built using Leptos and is located in `crates/frontend/`. Build configuration is defined in the workspace metadata (`Cargo.toml`).

### JavaScript Clients
```bash
cd clients/javascript
npm run install-all
npm run build
```

## Testing

### Rust Tests
```bash
# Run all tests
cargo test

# Run tests for specific crate
cargo test -p superposition_core
```

### Integration Tests (Postman/Newman)
```bash
# Run all API tests
npm test

# Load specific test collections
npm run load_superposition_tests
npm run load_exp_tests
npm run load_cac_tests
```

### Frontend E2E Tests
Located in `crates/frontend/end2end/`

## Database

- **Database**: PostgreSQL
- **ORM**: Diesel
- **Schema Files**:
  - `superposition.sql` - Main schema
  - `workspace_template.sql` - Multi-tenant workspace template

## Key Concepts

### Context-Aware Configuration (CAC)
Configurations that can vary based on context dimensions (e.g., user type, region, device). Contexts cascade from general to specific, similar to CSS specificity.

### Experimentation
Multi-variate experiment support for A/B testing configurations. Allows safe rollout of configuration changes.

### Multi-tenancy
Single deployment supports multiple isolated workspaces (formerly called tenants).

### Authorization
RBAC (Role-Based Access Control) support for managing who can make configuration/experimentation changes.

## API Documentation

The platform is built using AWS Smithy IDL, with definitions in the `smithy/` directory. All UI operations have corresponding API endpoints accessible via the SDK.

## Monitoring & Observability

- Grafana dashboards in `grafana/`
- Load testing with Locust (see `locust/`)
- Logging using structured logging with `log` crate

## Common Commands

```bash
# Format Rust code
cargo fmt

# Lint with Clippy
cargo clippy

# Run database migrations
# (specific commands depend on setup)

# Build Docker image
docker build -t superposition .

# Run with docker-compose
docker-compose up

# Access PostgreSQL directly
docker-compose exec postgres psql -U postgres
```

## Configuration Files

- `.env.example` - Environment variable template
- `Cargo.toml` - Rust workspace configuration
- `docker-compose.yaml` - Local development services
- `Dockerfile` - Production container image
- `rust-toolchain.toml` - Rust version specification
- `makefile` - Build automation tasks

## External Documentation

- Main docs: https://juspay.io/superposition/docs
- Context-Aware Config: https://juspay.io/superposition/docs/basic-concepts/context-aware-config
- Experimentation: https://juspay.io/superposition/docs/basic-concepts/experimentation
- Quick Start: https://juspay.io/superposition/docs/quick_start

## Contact

- Email: superposition@juspay.in
- Discord: https://discord.gg/jNeUJR9Bwr
- Twitter: @superpositionJP

## License

MIT OR Apache-2.0
