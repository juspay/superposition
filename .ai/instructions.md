# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Superposition is a cloud configuration and experimentation management platform built in Rust that allows software teams to manage configuration via a central location and run multi-variate experiments. The platform consists of three core components:

- **Context-Aware-Configuration (CAC)** - A flexible configuration management system that supports contextual overrides for configuration keys
- **Experimentation** - An experimentation management system for A/B testing with equal-sized cohorts
- **Metrics** - A metrics sub-system for analytics backends (TBD)

## Architecture

**Backend**: Rust-based microservices using Actix-Web framework with PostgreSQL database and Diesel ORM
**Frontend**: Leptos (Rust → WebAssembly) with Tailwind CSS for the admin UI
**Database**: PostgreSQL with comprehensive migrations in `crates/superposition_types/migrations/`
**Client SDKs**: Multi-language support (JavaScript/TypeScript, Python, Java, Go, Haskell) with Smithy-generated clients
**Infrastructure**: Docker Compose setup with PostgreSQL, LocalStack (AWS emulation), and Redis

## Common Development Commands

### Initial Setup
```bash
make setup                    # Complete development environment setup (database, dependencies, environment)
```

### Development
```bash
make run                     # Run Superposition in development mode with hot reload
make run_legacy             # Run in legacy mode
make superposition_dev      # Run with cargo watch for auto-recompilation
```

### Building
```bash
make build                  # Build both frontend (WASM) and backend
make frontend              # Build only frontend (Leptos → WASM + Tailwind CSS)
make backend               # Build only backend (Rust + Node.js dependencies)
make superposition         # Build the main binary
```

### Testing
```bash
make test                  # Run comprehensive test suite (unit tests + integration tests)
cargo test                 # Run Rust unit tests only
npm test                   # Run Newman API tests (requires server running)
```

### Database
```bash
make db                    # Start PostgreSQL container
make db-init              # Run database migrations
make migration            # Create and run new migration
make tenant TENANT=name   # Create new tenant
make schema-file          # Regenerate database schema files
```

### Client SDKs
```bash
make clients              # Build all client SDKs
make setup-clients        # Setup JavaScript/TypeScript client dependencies
make smithy-clients       # Generate clients from Smithy definitions
```

### Code Quality
```bash
make fmt                  # Format Rust code
make lint                 # Run clippy linting
make lint-fix             # Auto-fix lint issues
make check                # Run formatting and linting checks
```

### Infrastructure
```bash
make localstack           # Start LocalStack (AWS emulation)
make validate-aws-connection    # Test AWS/LocalStack connection
make validate-psql-connection   # Test PostgreSQL connection
make kill                 # Stop running processes
```

## Key Directories

- `crates/` - Rust workspace containing all core crates:
  - `superposition/` - Main binary and application entry point
  - `context_aware_config/` - CAC API implementation
  - `experimentation_platform/` - Experimentation API implementation  
  - `frontend/` - Leptos-based admin UI
  - `superposition_types/` - Shared types and database models
  - `cac_client/` - CAC client library
  - `experimentation_client/` - Experimentation client library
- `clients/` - Multi-language client SDKs
- `smithy/` - API definitions for client generation
- `docs/` - Documentation and guides
- `postman/` - API testing collections
- `examples/` - Example applications

## Configuration

Environment variables are managed through `.env` file (copied from `.env.example` during setup).

Key environment variables:
- `TENANTS` - List of tenants (default: `dev,test`)
- `DATABASE_URL` - PostgreSQL connection string
- `DOCKER_DNS` - DNS for containers (default: `localhost`)

## Development Workflow

1. **Setup**: `make setup` handles database, environment, and all dependencies
2. **Development**: `make run` starts the server with hot reload at `localhost:8080`
3. **Testing**: Use `make test` for comprehensive testing including API tests
4. **Client Development**: Client SDKs are auto-generated from Smithy definitions
5. **Database Changes**: Use diesel migrations in `crates/superposition_types/migrations/`

## Testing

The project has comprehensive testing:
- Rust unit tests: `cargo test`
- API integration tests: Newman collections in `postman/`
- Client SDK tests: Located in respective client directories
- End-to-end tests: Frontend tests in `crates/frontend/end2end/`

## Multi-language Support

Client SDKs are generated from Smithy API definitions:
- **JavaScript/TypeScript**: `clients/javascript/sdk/`
- **Python**: `clients/python/sdk/`
- **Java**: `clients/java/sdk/`
- **Go**: `clients/go/`
- **Haskell**: `clients/haskell/`
- **Rust**: `clients/generated/smithy/rust/`

All clients support both Context-Aware Configuration and Experimentation features.